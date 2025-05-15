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
  check_chat(chat)
  provider <- chat$get_provider()
  check_has_batch_support(provider)

  user_turns <- as_user_turns(prompts)
  check_string(path, allow_empty = FALSE)
  check_bool(wait)

  results <- batch_chat_process(chat, user_turns, path, wait = wait)

  map2(results, user_turns, function(result, user_turn) {
    ai_turn <- batch_result_turn(provider, result)
    if (!is.null(ai_turn)) {
      chat$clone()$add_turn(user_turn, ai_turn)
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
  check_has_batch_support(provider)

  user_turns <- as_user_turns(prompts)
  check_string(path, allow_empty = FALSE)
  check_bool(wait)

  needs_wrapper <- S7_inherits(provider, ProviderOpenAI)
  results <- batch_chat_process(
    chat,
    user_turns,
    path,
    type = wrap_type_if_needed(type, needs_wrapper),
    wait = wait
  )

  turns <- map(results, function(result) {
    batch_result_turn(provider, result, has_type = TRUE)
  })
  multi_convert(
    turns,
    type,
    needs_wrapper = needs_wrapper,
    convert = convert,
    include_tokens = include_tokens,
    include_cost = include_cost
  )
}

batch_chat_process <- function(
  chat,
  user_turns,
  path,
  type = NULL,
  wait = TRUE,
  error_call = caller_env()
) {
  provider <- chat$get_provider()

  if (file.exists(path)) {
    state <- jsonlite::read_json(path)
  } else {
    state <- list(stage = "submitting", batch = NULL, results = NULL)
  }

  if (state$stage == "submitting") {
    existing <- chat$get_turns(include_system_prompt = TRUE)
    conversations <- append_turns(list(existing), user_turns)
    state$batch <- batch_submit(provider, conversations, type = type)
    state$stage <- "waiting"
    save_state(state, path)
  }

  if (state$stage == "waiting") {
    # want to ensure that we always cycle once, even when wait = FALSE
    state$batch <- batch_poll(provider, state$batch)
    status <- batch_status(provider, state$batch)
    save_state(state, path)

    if (status$working && wait) {
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
        state$batch <- batch_poll(provider, state$batch)
        status <- batch_status(provider, state$batch)
        save_state(state, path)
      }
      cli::cli_process_done()
    }

    if (!status$working) {
      state$stage <- "retrieving"
      save_state(state, path)
    } else if (!wait) {
      cli::cli_abort("Batch is still processing.")
    } else {
      cli::cli_abort("Unexpected state", .internal = TRUE)
    }
  }

  if (state$stage == "retrieving") {
    state$results <- batch_retrieve(provider, state$batch)
    state$stage <- "done"
    save_state(state, path)
  }

  if (state$stage != "done") {
    cli::cli_abort("Unknown stage: {state$stage}", .internal = TRUE)
  }

  state$results
}

save_state <- function(state, path) {
  jsonlite::write_json(state, path, auto_unbox = TRUE, pretty = TRUE)
}

check_has_batch_support <- function(provider, call = caller_env()) {
  if (has_batch_support(provider)) {
    return(invisible())
  }

  cli::cli_abort(
    "Batch requests are not currently supported by this provider.",
    call = call
  )
}
