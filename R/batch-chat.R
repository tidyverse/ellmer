#' Submit multiple chats in one batch
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Some providers (i.e. OpenAI and anthropic) support a batched API where you
#' can submit many requests at once, and you get the results back within 24
#' hours. If you want to get results back more quickly, you may want to use
#' [parallel_chat()] instead.
#'
#' Batched requests are cheaper (usually 50% off), but ellmer does not yet
#' reflect this discount in its pricing.
#'
#' Since batched requests can take up to 24 hours to complete, `batch_chat()`
#' is designed to be resumable. You can either set `wait = FALSE` or simply
#' interrupt the waiting process, then later, call `batch_chat()` to resume
#' the process. It's up to you to delete the file once you're done with
#' it; ellmer will not delete it for you.
#' 
#' This API is marked as experimental, since I don't know how to helpfully 
#' deal with errors. Fortunately they don't seem to be common, but if you
#' have ideas, please let me know!
#'
#' @param inheritParams parallel_chat
#' @param path Path to file (with `.json` extension) to store state.
#' @param wait If `TRUE`, will wait for batch to complete. If `FALSE`,
#'   it will check once, and return `NULL` immediately.
#' @examples
#' chat <- chat_openai()
#'
#' # Chat ----------------------------------------------------------------------
#' country <- c("Canada", "New Zealand", "Jamaica", "United States")
#' prompts <- interpolate("What's the capital of {{country}}?")
#' chats <- batch_chat(chat, prompts, path = "capitals.json")
#' chats
batch_chat <- function(chat, prompts, path, wait = TRUE) {
  check_chat(chat)
  provider <- chat$get_provider()
  check_has_batch_support(provider)

  user_turns <- as_user_turns(prompts)

  if (file.exists(path)) {
    state <- jsonlite::read_json(path)
  } else {
    state <- list(stage = "submitting", batch = NULL, results = NULL)
  }
  save_state <- function() {
    jsonlite::write_json(
      state,
      path,
      auto_unbox = TRUE,
      pretty = TRUE
    )
  }

  if (state$stage == "submitting") {
    existing <- chat$get_turns(include_system_prompt = TRUE)
    conversations <- append_turns(list(existing), user_turns)
    state$batch <- batch_submit(provider, conversations)
    state$stage <- "waiting"
    save_state()
  }

  if (state$stage == "waiting") {
    # want to ensure that we always cycle once, even when wait = FALSE
    state$batch <- batch_poll(provider, state$batch)
    status <- batch_status(provider, state$batch)
    save_state()

    if (status$working && wait) {
      cli::cli_progress_bar(
        format = paste(
          "{cli::pb_spin} Processing... ",
          "{status$n_processing} -> {cli::col_green({status$n_succeeded})} / {cli::col_red({status$n_failed})} ",
          "[{cli::pb_elapsed}]"
        ),
        clear = FALSE
      )
      while (status$working) {
        Sys.sleep(0.5)
        cli::cli_progress_update()
        state$batch <- batch_poll(provider, state$batch)
        status <- batch_status(provider, state$batch)
        save_state()
      }
      cli::cli_process_done()
    }

    if (!status$working) {
      state$stage <- "retrieving"
      save_state()
    } else if (!wait) {
      cli::cli_abort("Batch is still processing.")
    } else {
      cli::cli_abort("Unexpected state", .internal = TRUE)
    }
  }

  if (state$stage == "retrieving") {
    state$results <- batch_retrieve(provider, batch)
    state$stage <- "done"
    save_state()
  }

  if (state$stage != "done") {
    cli::cli_abort("Unknown stage: {state$stage}", .internal = TRUE)
  }
  map2(state$results, user_turns, function(result, user_turn) {
    ai_turn <- batch_result_turn(provider, result)
    if (!is.null(ai_turn)) {
      chat$clone()$add_turn(user_turn, ai_turn)
    } else {
      NULL
    }
  })
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
