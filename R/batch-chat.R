#' Submit multiple chats in one batch
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `batch_chat()` and `batch_chat_structured()` currently only work with
#' [chat_openai()] and [chat_anthropic()]. They use the
#' [OpenAI](https://platform.openai.com/docs/guides/batch) and
#' [Anthropic](https://docs.anthropic.com/en/docs/build-with-claude/batch-processing)
#' batch APIs which allow you to submit multiple requests simultaneously.
#' The results can take up to 24 hours to complete, but in return you pay 50%
#' less than usual (but note that ellmer doesn't include this discount in
#' its pricing metadata). If you want to get results back more quickly, or
#' you're working with a different provider, you may want to use
#' [parallel_chat()] instead.
#'
#' Since batched requests can take a long time to complete, `batch_chat()`
#' requires a file path that is used to store information about the batch so
#' you never lose any work. You can either set `wait = FALSE` or simply
#' interrupt the waiting process, then later, either call `batch_chat()` to
#' resume where you left off or call `batch_chat_completed()` to see if the
#' results are ready to retrieve. `batch_chat()` will store the chat responses
#' in this file, so you can either keep it around to cache the results,
#' or delete it to free up disk space.
#'
#' This API is marked as experimental since I don't yet know how to handle
#' errors in the most helpful way. Fortunately they don't seem to be common,
#' but if you have ideas, please let me know!
#'
#' @inheritParams parallel_chat
#' @param path Path to file (with `.json` extension) to store state.
#'
#'   The file records a hash of the provider, the prompts, and the existing
#'   chat turns. If you attempt to reuse the same file with any of these being
#'   different, you'll get an error.
#' @param wait If `TRUE`, will wait for batch to complete. If `FALSE`,
#'   it will return `NULL` if the batch is not complete, and you can retrieve
#'   the results later by re-running `batch_chat()` when
#'   `batch_chat_completed()` is `TRUE`.
#' @examplesIf has_credentials("openai")
#' chat <- chat_openai(model = "gpt-4.1-nano")
#'
#' # Chat ----------------------------------------------------------------------
#'
#' prompts <- interpolate("What do people from {{state.name}} bring to a potluck dinner?")
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
#' @export
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
  job$step_until_done()
  turns <- job$result_turns()

  multi_convert(
    provider,
    turns,
    type,
    convert = convert,
    include_tokens = include_tokens,
    include_cost = include_cost
  )
}

#' @export
#' @rdname batch_chat
batch_chat_completed <- function(chat, prompts, path) {
  job <- BatchJob$new(
    chat = chat,
    prompts = prompts,
    path = path
  )
  switch(
    job$stage,
    "submitting" = FALSE,
    "waiting" = !job$poll()$working,
    "retrieving" = TRUE,
    "done" = TRUE,
    cli::cli_abort("Unexpected stage: {job$stage}", .internal = TRUE)
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
    started_at = NULL,
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
        self$started_at <- .POSIXct(state$started_at)

        self$check_hash(state$hash, call = call)
      } else {
        self$stage <- "submitting"
        self$batch <- NULL
        self$started_at <- Sys.time()
      }
    },

    save_state = function() {
      jsonlite::write_json(
        list(
          version = 1,
          stage = self$stage,
          batch = self$batch,
          results = self$results,
          started_at = as.integer(self$started_at),
          hash = self$compute_hash()
        ),
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
        if (!self$step()) {
          return(invisible())
        }
      }
      invisible(self)
    },

    submit = function() {
      existing <- self$chat$get_turns(include_system_prompt = TRUE)
      conversations <- append_turns(list(existing), self$user_turns)

      self$batch <- batch_submit(self$provider, conversations, type = self$type)
      self$stage <- "waiting"
      self$save_state()
      TRUE
    },

    wait = function() {
      # always poll once, even when wait = FALSE
      status <- self$poll()

      if (self$should_wait) {
        cli::cli_progress_bar(
          format = paste(
            "{cli::pb_spin} Processing...",
            "[{self$elapsed()}]",
            "{status$n_processing} pending |",
            "{cli::col_green({status$n_succeeded})} done |",
            "{cli::col_red({status$n_failed})} failed"
          )
        )
        while (status$working) {
          Sys.sleep(0.5)
          cli::cli_progress_update()
          status <- self$poll()
        }
        cli::cli_progress_done()
      }

      if (!status$working) {
        self$stage <- "retrieving"
        self$save_state()
        TRUE
      } else {
        FALSE
      }
    },
    poll = function() {
      self$batch <- batch_poll(self$provider, self$batch)
      self$save_state()

      batch_status(self$provider, self$batch)
    },
    elapsed = function() {
      pretty_sec(as.integer(Sys.time()) - as.integer(self$started_at))
    },

    retrieve = function() {
      self$results <- batch_retrieve(self$provider, self$batch)
      self$stage <- "done"
      self$save_state()
      TRUE
    },

    result_turns = function() {
      map2(self$results, self$user_turns, function(result, user_turn) {
        batch_result_turn(self$provider, result, has_type = !is.null(self$type))
      })
    },

    compute_hash = function() {
      # TODO: replace with JSON serialization when available
      list(
        provider = hash(props(self$provider)),
        prompts = hash(lapply(self$user_turns, format)),
        user_turns = hash(lapply(self$chat$get_turns(TRUE), format))
      )
    },

    check_hash = function(old_hash, call = caller_env()) {
      new_hash <- self$compute_hash()
      same <- map2_lgl(old_hash, new_hash, `==`)

      if (all(same)) {
        return(invisible())
      }
      differences <- names(new_hash)[!same]

      cli::cli_abort(
        c(
          "{differences} don't match stored values.",
          i = "Do you need to pick a different {.arg path}?"
        ),
        call = call
      )
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
