#' Submit multiple chats in one batch
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `batch_chat()` and `batch_chat_structured()` currently only works with
#' [chat_openai()] and [chat_anthropic()]. They use the OpenAI and Anthropic
#' batch APIs which allow you to submit multiple requests simultaenously.
#' The results can take up to 24 hours to come back, but in return you pay 50%
#' less than usual (but note that ellmer doesn't include this discount in
#' its pricing metadata). If you want to get results back more quickly, or
#' you're working with a different provider, you may want to use
#' [parallel_chat()] instead.
#'
#' Since batched requests can take a long time to complete, `batch_chat()`
#' requires a file path that is used to store information about the batch so
#' you never lose any work. You can either set `wait = FALSE` or simply
#' interrupt the waiting process, then later, call `batch_chat()` to resume
#' where you left off. Once you're done with the chat results, delete the
#' file you created to avoid re-using previous results.
#'
#' This API is marked as experimental, since I don't know how to helpfully
#' deal with errors. Fortunately they don't seem to be common, but if you
#' have ideas, please let me know!
#'
#' @inheritParams parallel_chat
#' @param path Path to file (with `.json` extension) to store state.
#' @param wait If `TRUE`, will wait for batch to complete. If `FALSE`,
#'   it will check once and error if the job is not complete.
#' @examplesIf has_credentials("openai")
#' chat <- chat_openai(model = "gpt-4.1-nano")
#'
#' # Chat ----------------------------------------------------------------------
#'
#  prompts <- interpolate("What do people from {{state.name}} bring to a potluck dinner?")
#' \dontrun{
#' chats <- batch_chat(chat, prompts, path = "potluck.json")
#' chats
#' }
#'
#' # Structured data -----------------------------------------------------------
#' prompts <- list(
#'   "I go by Alex. 42 years on this planet and counting.",
#'   "Pleased to meet you! I'm Jamal, age 27.",
#'   "They call me Li Wei. Nineteen years young.",
#'   "Fatima here. Just celebrated my 35th birthday last week.",
#'   "The name's Robert - 51 years old and proud of it.",
#'   "Kwame here - just hit the big 5-0 this year."
#' )
#' type_person <- type_object(name = type_string(), age = type_number())
#' \dontrun{
#' data <- batch_chat_structured(
#'   chat = chat,
#'   prompts = prompts,
#'   path = "people-data.json",
#'   type = type_person
#' )
#' data
#' }
#'
batch_chat <- function(chat, prompts, path, wait = TRUE) {
  job <- BatchJob$new(
    chat = chat,
    prompts = prompts,
    path = path,
    wait = wait
  )
  job$step_until_done()

  assistant_turns <- job$result_turns()
  map2(job$user_turns, assistant_turns, function(user, assistant) {
    if (!is.null(assistant)) {
      chat$clone()$add_turn(user, assistant)
    } else {
      NULL
    }
  })
}

#' @export
#' @rdname batch_chat
#' @inheritParams parallel_chat_structured
batch_chat_structured <- function(
  chat,
  prompts,
  path,
  type,
  wait = TRUE,
  convert = TRUE,
  include_tokens = FALSE,
  include_cost = FALSE
) {
  check_chat(chat)
  provider <- chat$get_provider()
  needs_wrapper <- S7_inherits(provider, ProviderOpenAI)

  job <- BatchJob$new(
    chat = chat,
    prompts = prompts,
    type = wrap_type_if_needed(type, needs_wrapper),
    path = path,
    wait = wait,
    call = error_call
  )
  results <- job$step_until_done()
  turns <- job$result_turns()

  multi_convert(
    turns,
    type,
    needs_wrapper = needs_wrapper,
    convert = convert,
    include_tokens = include_tokens,
    include_cost = include_cost
  )
}

BatchJob <- R6::R6Class(
  "BatchJob",
  public = list(
    chat = NULL,
    user_turns = NULL,
    path = NULL,
    should_wait = TRUE,
    type = NULL,

    # Internal state
    provider = NULL,
    stage = NULL,
    batch = NULL,
    results = NULL,

    initialize = function(
      chat,
      prompts,
      path,
      type = NULL,
      wait = TRUE,
      call = caller_env(2)
    ) {
      check_chat(chat, call = call)
      self$provider <- chat$get_provider()
      check_has_batch_support(self$provider, call = call)

      user_turns <- as_user_turns(prompts, call = call)
      check_string(path, allow_empty = FALSE, call = call)
      check_bool(wait, call = call)

      self$chat <- chat
      self$user_turns <- user_turns
      self$type <- type
      self$path <- path
      self$should_wait <- wait

      if (file.exists(path)) {
        state <- jsonlite::read_json(path)
        self$stage <- state$stage
        self$batch <- state$batch
        self$results <- state$results
      } else {
        self$stage <- "submitting"
        self$batch <- NULL
      }
    },

    save_state = function() {
      jsonlite::write_json(
        list(stage = self$stage, batch = self$batch, results = self$results),
        self$path,
        auto_unbox = TRUE,
        pretty = TRUE
      )
    },

    step = function() {
      if (self$stage == "submitting") {
        self$submit()
      } else if (self$stage == "waiting") {
        self$wait()
      } else if (self$stage == "retrieving") {
        self$retrieve()
      } else {
        cli::cli_abort("Unknown stage: {self$stage}", .internal = TRUE)
      }
    },

    step_until_done = function() {
      while (self$stage != "done") {
        self$step()
      }
      self$results
    },

    submit = function() {
      existing <- self$chat$get_turns(include_system_prompt = TRUE)
      conversations <- append_turns(list(existing), self$user_turns)

      self$batch <- batch_submit(self$provider, conversations, type = self$type)
      self$stage <- "waiting"
      self$save_state()
    },

    wait = function() {
      # want to ensure that we always cycle once, even when wait = FALSE
      self$batch <- batch_poll(self$provider, self$batch)
      status <- batch_status(self$provider, self$batch)
      self$save_state()

      if (status$working && self$should_wait) {
        cli::cli_progress_bar(
          format = paste(
            "{cli::pb_spin} Processing... ",
            "{status$n_processing} -> {cli::col_green({status$n_succeeded})} / {cli::col_red({status$n_failed})} ",
            "[{cli::pb_elapsed}]"
          )
        )
        while (status$working) {
          Sys.sleep(0.5)
          cli::cli_progress_update()
          self$batch <- batch_poll(self$provider, self$batch)
          status <- batch_status(self$provider, self$batch)
          self$save_state()
        }
        cli::cli_process_done()
      }

      if (!status$working) {
        self$stage <- "retrieving"
        self$save_state()
      } else if (!self$should_wait) {
        cli::cli_abort("Batch is still processing.")
      } else {
        cli::cli_abort("Unexpected state", .internal = TRUE)
      }
    },

    retrieve = function() {
      self$results <- batch_retrieve(self$provider, self$batch)
      self$stage <- "done"
      self$save_state()
    },

    result_turns = function() {
      map2(self$results, self$user_turns, function(result, user_turn) {
        batch_result_turn(self$provider, result, has_type = !is.null(self$type))
      })
    }
  )
)


check_has_batch_support <- function(provider, call = caller_env()) {
  if (has_batch_support(provider)) {
    return(invisible())
  }

  cli::cli_abort(
    "Batch requests are not currently supported by this provider.",
    call = call
  )
}
