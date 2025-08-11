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
#' @param chat A base chat object.
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
#' @returns
#' For `parallel_chat()`, a list of [Chat] objects, one for each prompt.
#' For `parallel_chat_text()`, a character vector of text responses.
#' For `parallel_chat_structured()`, a single structured data object with one
#' element for each prompt. Typically, when `type` is an object, this will
#' will be a data frame with one row for each prompt, and one column for each
#' property.
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
parallel_chat <- function(chat, prompts, max_active = 10, rpm = 500) {
  check_chat(chat)
  my_parallel_turns <- function(conversations) {
    parallel_turns(
      provider = chat$get_provider(),
      conversations = conversations,
      tools = chat$get_tools(),
      max_active = max_active,
      rpm = rpm
    )
  }

  # First build up list of cumulative conversations
  user_turns <- as_user_turns(prompts)
  existing <- chat$get_turns(include_system_prompt = TRUE)
  conversations <- append_turns(list(existing), user_turns)

  # Now get the assistant's response
  assistant_turns <- my_parallel_turns(conversations)
  conversations <- append_turns(conversations, assistant_turns)

  repeat {
    assistant_turns <- map(
      assistant_turns,
      \(turn) match_tools(turn, tools = chat$get_tools())
    )
    tool_results <- map(
      assistant_turns,
      \(turn) coro::collect(invoke_tools(turn))
    )
    user_turns <- map(tool_results, tool_results_as_turn)
    needs_iter <- !map_lgl(user_turns, is.null)
    if (!any(needs_iter)) {
      break
    }

    # don't need to index because user_turns null
    conversations <- append_turns(conversations, user_turns)

    assistant_turns <- vector("list", length(user_turns))
    assistant_turns[needs_iter] <- my_parallel_turns(conversations[needs_iter])
    conversations <- append_turns(conversations, assistant_turns)
  }

  map(conversations, \(turns) chat$clone()$set_turns(turns))
}

#' @rdname parallel_chat
#' @export
parallel_chat_text <- function(chat, prompts, max_active = 10, rpm = 500) {
  chats <- parallel_chat(chat, prompts, max_active = max_active, rpm = rpm)
  map_chr(chats, \(chat) chat$last_turn()@text)
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
  rpm = 500
) {
  turns <- as_user_turns(prompts)
  check_bool(convert)

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
    rpm = rpm
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

#' @description
#' [parallel_chat_structured_robust()] is similar to
#' [parallel_chat_structured()], but continues processing even when individual
#' prompts fail. Failed prompts return proper NA structures that match the type
#' specification instead of stopping the entire process.
#'
#' @param include_status If `TRUE`, and the result is a data frame, will
#'   add `status` column with `"success"` for successful prompts and error
#'   messages for failed prompts.
#' @param on_error Character string specifying how to handle errors:
#'   `"continue"` (default) continues processing all prompts, returning NA for failed ones (maintains input length),
#'   `"return"` stops at first error and returns only successful results processed before the error, printing the error message,
#'   `"stop"` stops on first error like the original function.
#' @returns [parallel_chat_structured_robust()] returns the same as
#'   [parallel_chat_structured()], but with NA values for failed prompts instead
#'   of throwing an error. For `on_error="continue"`, output length always matches
#'   input length. For `on_error="return"`, returns only successful results processed
#'   before the first error.
#' @export
#' @rdname parallel_chat
parallel_chat_structured_robust <- function(
  chat,
  prompts,
  type,
  convert = TRUE,
  include_tokens = FALSE,
  include_cost = FALSE,
  include_status = FALSE,
  max_active = 10,
  rpm = 500,
  on_error = c("continue", "return", "stop")
) {
  on_error <- arg_match(on_error)
  if (on_error == "stop") {
    # For fallback to original function, add status column if requested
    result <- parallel_chat_structured(
      chat = chat,
      prompts = prompts,
      type = type,
      convert = convert,
      include_tokens = include_tokens,
      include_cost = include_cost,
      max_active = max_active,
      rpm = rpm
    )

    # Add status column with all "success" since original function stops on error
    if (include_status && is.data.frame(result)) {
      result$status <- rep("success", nrow(result))
    }
    return(result)
  }

  turns <- as_user_turns(prompts)
  check_bool(convert)

  provider <- chat$get_provider()
  needs_wrapper <- type_needs_wrapper(type, provider)

  # First build up list of cumulative conversations
  user_turns <- as_user_turns(prompts)
  existing <- chat$get_turns(include_system_prompt = TRUE)
  conversations <- append_turns(list(existing), user_turns)

  turns <- parallel_turns_robust(
    provider = provider,
    conversations = conversations,
    tools = chat$get_tools(),
    type = wrap_type_if_needed(type, needs_wrapper),
    max_active = max_active,
    rpm = rpm,
    on_error = on_error
  )

  result <- multi_convert_robust(
    provider,
    turns,
    type,
    convert = convert,
    include_tokens = include_tokens,
    include_cost = include_cost,
    include_status = include_status,
    on_error = on_error
  )

  # For "continue" mode, check if there were any errors and warn user
  if (on_error == "continue") {
    error_turns <- map_lgl(turns, ~ inherits(.x, "error_turn"))
    if (any(error_turns)) {
      error_indices <- which(error_turns)
      error_messages <- map_chr(turns[error_turns], ~ .x$error)

      # Issue warnings for each error so they can be retrieved with warnings()
      for (i in seq_along(error_indices)) {
        warning(paste0("Prompt ", error_indices[i], ": ", error_messages[i]),
                call. = FALSE, immediate. = FALSE)
      }

      # Issue a summary message that will be immediately visible
      n_errors <- length(error_indices)
      message("Some prompts produced errors (", n_errors, " out of ", length(prompts), "). Use warnings() to see error details.")
    }
  }

  result
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
    extract_data(
      turn = turn,
      type = wrap_type_if_needed(type, needs_wrapper),
      convert = FALSE,
      needs_wrapper = needs_wrapper
    )
  })

  if (convert) {
    out <- convert_from_type(rows, type_array(type))
  } else {
    out <- rows
  }

  if (is.data.frame(out) && (include_tokens || include_cost)) {
    tokens <- t(vapply(turns, \(turn) turn@tokens, integer(3)))

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
  out
}

multi_convert_robust <- function(
  provider,
  turns,
  type,
  convert = TRUE,
  include_tokens = FALSE,
  include_cost = FALSE,
  include_status = FALSE,
  on_error = "continue"
) {
  needs_wrapper <- type_needs_wrapper(type, provider)

  # Process turns and collect both data and status
  results <- map(turns, function(turn) {
    if (inherits(turn, "error_turn")) {
      # Create proper NA structure for failed prompts
      list(
        data = create_na_structure(type, needs_wrapper),
        status = turn$error
      )
    } else {
      tryCatch({
        data <- extract_data(
          turn = turn,
          type = wrap_type_if_needed(type, needs_wrapper),
          convert = FALSE,
          needs_wrapper = needs_wrapper
        )
        list(data = data, status = "success")
      }, error = function(e) {
        # If extraction fails, return NA structure
        list(
          data = create_na_structure(type, needs_wrapper),
          status = conditionMessage(e)
        )
      })
    }
  })

  # For "return" mode, only include non-NULL results (successful ones)
  if (on_error == "return") {
    non_null_indices <- !map_lgl(turns, is.null)
    results <- results[non_null_indices]
    turns <- turns[non_null_indices]
    # Further filter to only successful results
    successful_indices <- map_lgl(results, ~ .x$status == "success")
    results <- results[successful_indices]
    turns <- turns[successful_indices]
  }

  # Extract data and status separately
  rows <- map(results, ~ .x$data)
  status_info <- map_chr(results, ~ .x$status)

  if (convert) {
    out <- convert_from_type(rows, type_array(type))
  } else {
    out <- rows
  }

  if (is.data.frame(out) && (include_tokens || include_cost || include_status)) {
    # Handle token information for both successful and failed turns
    if (include_tokens || include_cost) {
      tokens <- t(vapply(turns, function(turn) {
        if (inherits(turn, "error_turn")) {
          # Return zeros for failed turns
          c(0L, 0L, 0L)
        } else {
          turn@tokens
        }
      }, integer(3)))

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

    # Add status column if requested
    if (include_status) {
      out$status <- status_info
    }
  }
  out
}

create_na_structure <- function(type, needs_wrapper = FALSE) {
  actual_type <- if (needs_wrapper) type@properties[[1]] else type

  if (S7_inherits(actual_type, TypeObject)) {
    # For type objects, create NA values for each property
    prop_names <- names(actual_type@properties)
    structure(
      lapply(prop_names, function(prop_name) {
        prop_type <- actual_type@properties[[prop_name]]
        create_na_for_type(prop_type)
      }),
      names = prop_names
    )
  } else if (S7_inherits(actual_type, TypeArray)) {
    # For arrays, return empty list or appropriate structure
    list()
  } else {
    # For basic types
    create_na_for_type(actual_type)
  }
}

create_na_for_type <- function(type_spec) {
  if (S7_inherits(type_spec, TypeBasic)) {
    switch(type_spec@type,
      "string" = NA_character_,
      "integer" = NA_integer_,
      "number" = NA_real_,
      "boolean" = NA,
      NA
    )
  } else if (S7_inherits(type_spec, TypeArray)) {
    list()
  } else if (S7_inherits(type_spec, TypeObject)) {
    create_na_structure(type_spec, needs_wrapper = FALSE)
  } else {
    NA
  }
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

parallel_turns <- function(
  provider,
  conversations,
  tools,
  type = NULL,
  max_active = 10,
  rpm = 60
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

  resps <- req_perform_parallel(reqs, max_active = max_active)
  if (any(map_lgl(resps, is.null))) {
    cli::cli_abort("Terminated by user")
  }

  map(resps, function(resp) {
    json <- resp_body_json(resp)
    value_turn(provider, json, has_type = !is.null(type))
  })
}

parallel_turns_robust <- function(
  provider,
  conversations,
  tools,
  type = NULL,
  max_active = 10,
  rpm = 60,
  on_error = "continue"
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

  # For "return" mode, we need to process sequentially to catch the first error
  if (on_error == "return") {
    # Process requests sequentially until we hit an error
    results <- vector("list", length(reqs))
    first_error_index <- NULL
    first_error_message <- NULL

    for (i in seq_along(reqs)) {
      tryCatch({
        resp <- httr2::req_perform(reqs[[i]])
        json <- resp_body_json(resp)
        results[[i]] <- value_turn(provider, json, has_type = !is.null(type))
      }, error = function(e) {
        if (is.null(first_error_index)) {
          first_error_index <<- i
          first_error_message <<- conditionMessage(e)
        }
        results[[i]] <<- structure(
          list(
            error = conditionMessage(e),
            index = i,
            type_spec = type
          ),
          class = "error_turn"
        )
      })

      # Stop at first error
      if (!is.null(first_error_index)) {
        # Print the error message
        cli::cli_warn("Error in prompt {first_error_index}: {first_error_message}")
        cli::cli_inform("Returning results for first {first_error_index - 1} successful prompt{?s}")
        break
      }
    }

    return(results)
  } else {
    # Use httr2's parallel processing for "continue" and "stop" modes
    httr2_on_error <- if (on_error == "continue") "continue" else "stop"
    resps <- req_perform_parallel(reqs, max_active = max_active, on_error = httr2_on_error)

    # Check for user termination
    if (any(map_lgl(resps, is.null))) {
      cli::cli_abort("Terminated by user")
    }

    # Process responses with individual error handling
    return(map(seq_along(resps), function(i) {
      resp <- resps[[i]]

      if (inherits(resp, "error")) {
        # Return a special error turn that will be handled downstream
        structure(
          list(
            error = conditionMessage(resp),
            index = i,
            type_spec = type
          ),
          class = "error_turn"
        )
      } else {
        # Process successful response
        tryCatch({
          json <- resp_body_json(resp)
          value_turn(provider, json, has_type = !is.null(type))
        }, error = function(e) {
          # Return a special error turn for JSON parsing errors
          structure(
            list(
              error = conditionMessage(e),
              index = i,
              type_spec = type
            ),
            class = "error_turn"
          )
        })
      }
    }))
  }
}
