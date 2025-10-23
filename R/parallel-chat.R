#' Submit multiple chats in parallel
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' If you have multiple prompts, you can submit them in parallel. This is
#' typically considerably faster than submitting them in sequence, especially
#' with Gemini and OpenAI.
#'
#' If you're using [chat_openai()] or [chat_anthropic()] and you're willing
#' to wait longer, you might want to use [batch_chat()] instead, as it comes
#' with a 50% discount in return for taking up to 24 hours.
#'
#' @param chat A chat object created by a `chat_` function, or a
#'   string passed to [chat()].
#' @param prompts A vector created by [interpolate()] or a list
#'   of character vectors.
#' @param max_active The maximum number of simultaneous requests to send.
#'
#'   For [chat_anthropic()], note that the number of active connections is
#'   limited primarily by the output tokens per minute limit (OTPM) which is
#'   estimated from the `max_tokens` parameter, which defaults to 4096. That
#'   means if your usage tier limits you to 16,000 OTPM, you should either set
#'   `max_active = 4` (16,000 / 4096) to decrease the number of active
#'   connections or use [params()] in `chat_anthropic()` to decrease
#'   `max_tokens`.
#' @param rpm Maximum number of requests per minute.
#' @param on_error What to do when a request fails. One of:
#'   * `"return"` (the default): stop processing new requests, wait for
#'      in flight requests to finish, then return.
#'   * `"continue"`: keep going, performing every request.
#'   * `"stop"`: stop processing and throw an error.
#' @returns
#' For `parallel_chat()`, a list with one element for each prompt. Each element
#' is either a [Chat] object (if successful), a `NULL` (if the request wasn't
#' performed) or an error object (if it failed).
#'
#' For `parallel_chat_text()`, a character vector with one element for each
#' prompt. Requests that weren't succesful get an `NA`.
#'
#' For `parallel_chat_structured()`, a single structured data object with one
#' element for each prompt. Typically, when `type` is an object, this will
#' be a tibble with one row for each prompt, and one column for each
#' property. If the output is a data frame, and some requests error,
#' an `.error` column will be added with the error objects.
#' @export
#' @examples
#' \dontshow{ellmer:::vcr_example_start("parallel_chat")}
#' chat <- chat_openai()
#'
#' # Chat ----------------------------------------------------------------------
#' country <- c("Canada", "New Zealand", "Jamaica", "United States")
#' prompts <- interpolate("What's the capital of {{country}}?")
#' parallel_chat(chat, prompts)
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
#' parallel_chat_structured(chat, prompts, type_person)
#' \dontshow{ellmer:::vcr_example_end()}
parallel_chat <- function(
  chat,
  prompts,
  max_active = 10,
  rpm = 500,
  on_error = c("return", "continue", "stop")
) {
  chat <- as_chat(chat)
  on_error <- arg_match(on_error)

  my_parallel_turns <- function(conversations) {
    parallel_turns(
      provider = chat$get_provider(),
      conversations = conversations,
      tools = chat$get_tools(),
      max_active = max_active,
      rpm = rpm,
      on_error = on_error
    )
  }

  # First build up list of cumulative conversations
  user_turns <- as_user_turns(prompts)
  existing <- chat$get_turns(include_system_prompt = TRUE)
  conversations <- append_turns(list(existing), user_turns)

  # Now get the assistant's response
  assistant_turns <- my_parallel_turns(conversations)

  is_ok <- !map_lgl(assistant_turns, turn_failed)
  repeat {
    if (!any(is_ok)) {
      break
    }
    conversations[is_ok] <- append_turns(
      conversations[is_ok],
      assistant_turns[is_ok]
    )

    tool_turns <- map(assistant_turns[is_ok], function(turn) {
      turn <- match_tools(turn, tools = chat$get_tools())
      tool_results <- coro::collect(invoke_tools(turn))
      tool_results_as_turn(tool_results)
    })
    needs_iter <- !map_lgl(tool_turns, is.null)
    if (!any(needs_iter)) {
      break
    }

    conversations[is_ok][needs_iter] <- append_turns(
      conversations[is_ok][needs_iter],
      tool_turns[needs_iter]
    )

    assistant_turns <- vector("list", length(user_turns))
    assistant_turns[needs_iter] <- my_parallel_turns(conversations[needs_iter])
    is_ok[needs_iter] <- !map_lgl(assistant_turns[needs_iter], turn_failed)
  }

  map(seq_along(conversations), function(i) {
    if (is_ok[[i]]) {
      turns <- conversations[[i]]
      chat$clone()$set_turns(turns)
    } else {
      assistant_turns[[i]]
    }
  })
}

#' @rdname parallel_chat
#' @export
parallel_chat_text <- function(
  chat,
  prompts,
  max_active = 10,
  rpm = 500,
  on_error = c("return", "continue", "stop")
) {
  chat <- as_chat(chat)
  on_error <- arg_match(on_error)

  chats <- parallel_chat(
    chat,
    prompts,
    max_active = max_active,
    rpm = rpm,
    on_error = on_error
  )

  is_ok <- !map_lgl(chats, turn_failed)
  out <- rep(NA_character_, length(prompts))
  out[is_ok] <- map_chr(chats[is_ok], \(chat) chat$last_turn()@text)
  out
}

#' @param type A type specification for the extracted data. Should be
#'   created with a [`type_()`][type_boolean] function.
#' @param convert If `TRUE`, automatically convert from JSON lists to R
#'   data types using the schema. This typically works best when `type` is
#'   [type_object()] as this will give you a data frame with one column for
#'   each property. If `FALSE`, returns a list.
#' @param include_tokens If `TRUE`, and the result is a data frame, will
#'   add `input_tokens` and `output_tokens` columns giving the total input
#'   and output tokens for each prompt.
#' @param include_cost If `TRUE`, and the result is a data frame, will
#'   add `cost` column giving the cost of each prompt.
#' @export
#' @rdname parallel_chat
parallel_chat_structured <- function(
  chat,
  prompts,
  type,
  convert = TRUE,
  include_tokens = FALSE,
  include_cost = FALSE,
  max_active = 10,
  rpm = 500,
  on_error = c("return", "continue", "stop")
) {
  chat <- as_chat(chat)
  turns <- as_user_turns(prompts)
  check_bool(convert)
  on_error <- arg_match(on_error)

  provider <- chat$get_provider()
  needs_wrapper <- type_needs_wrapper(type, provider)

  # First build up list of cumulative conversations
  user_turns <- as_user_turns(prompts)
  existing <- chat$get_turns(include_system_prompt = TRUE)
  conversations <- append_turns(list(existing), user_turns)

  turns <- parallel_turns(
    provider = provider,
    conversations = conversations,
    tools = chat$get_tools(),
    type = wrap_type_if_needed(type, needs_wrapper),
    max_active = max_active,
    rpm = rpm,
    on_error = on_error
  )

  multi_convert(
    provider,
    turns,
    type,
    convert = convert,
    include_tokens = include_tokens,
    include_cost = include_cost
  )
}

multi_convert <- function(
  provider,
  turns,
  type,
  convert = TRUE,
  include_tokens = FALSE,
  include_cost = FALSE
) {
  needs_wrapper <- type_needs_wrapper(type, provider)

  rows <- map(turns, \(turn) {
    if (turn_failed(turn)) {
      NULL
    } else {
      safely(
        extract_data(
          turn = turn,
          type = wrap_type_if_needed(type, needs_wrapper),
          convert = FALSE,
          needs_wrapper = needs_wrapper
        )
      )
    }
  })

  is_err <- map_lgl(rows, \(x) !is.null(x$error))
  n_error <- sum(is_err)
  if (n_error > 0) {
    msgs <- map(rows[is_err], \(x) conditionMessage(x$error))
    errors <- paste0(" * ", seq_along(turns)[is_err], ": ", msgs)

    cli::cli_warn(c(
      "Failed to extract data from {n_error}/{length(turns)} turns",
      cli_escape(errors)
    ))
  }
  # convert_from_type() will convert NULL to required type
  row_data <- map(rows, \(x) x$result)

  if (convert) {
    out <- convert_from_type(row_data, type_array(type))
  } else {
    out <- row_data
  }

  if (is.data.frame(out)) {
    is_error <- map_lgl(turns, turn_failed)
    if (any(is_error)) {
      errors <- vector("list", length(turns))
      errors[is_error] <- turns[is_error]
      out$.error <- errors
    }

    if (include_tokens || include_cost) {
      tokens <- t(vapply(
        turns,
        \(turn) if (turn_failed(turn)) c(0L, 0L, 0L) else turn@tokens,
        integer(3)
      ))

      if (include_tokens) {
        out$input_tokens <- tokens[, 1]
        out$output_tokens <- tokens[, 2]
        out$cached_input_tokens <- tokens[, 3]
      }

      if (include_cost) {
        out$cost <- get_token_cost(
          provider@name,
          provider@model,
          input = tokens[, 1],
          output = tokens[, 2],
          cached_input = tokens[, 3]
        )
      }
    }
  }
  out
}

append_turns <- function(old_turns, new_turns) {
  map2(old_turns, new_turns, function(old, new) {
    if (is.null(new)) {
      old
    } else {
      c(old, list(new))
    }
  })
}

turn_failed <- function(turn) {
  is.null(turn) || inherits(turn, "error")
}

parallel_turns <- function(
  provider,
  conversations,
  tools,
  type = NULL,
  max_active = 10,
  rpm = 60,
  on_error = "return"
) {
  reqs <- map(conversations, function(turns) {
    chat_request(
      provider = provider,
      turns = turns,
      type = type,
      tools = tools,
      stream = FALSE
    )
  })
  reqs <- map(reqs, function(req) {
    req_throttle(req, capacity = rpm, fill_time_s = 60)
  })

  # Returns list where elements NULL, an error, or a response
  resps <- req_perform_parallel(
    reqs,
    max_active = max_active,
    on_error = on_error
  )

  is_absent <- map_lgl(resps, is.null)
  if (any(is_absent)) {
    n <- sum(is_absent)
    cli::cli_warn("{n} request{?s} did not complete.")
  }

  is_error <- map_lgl(resps, inherits, "error")
  if (any(is_error)) {
    n <- sum(is_error)
    cli::cli_warn("{n} request{?s} errored.")
  }

  map(resps, function(resp) {
    if (is.null(resp)) {
      NULL
    } else if (inherits(resp, "error")) {
      resp
    } else {
      json <- resp_body_json(resp)
      turn <- value_turn(provider, json, has_type = !is.null(type))
      turn@duration <- resp_timing(resp)[["total"]] %||% NA_real_
      turn
    }
  })
}

# Helpers -----------------------------------------------------------------

safely <- function(code) {
  tryCatch(
    list(result = code, error = NULL),
    error = function(cnd) {
      list(result = NULL, error = cnd)
    }
  )
}
