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
#' @return
#' For `parallel_chat()`, a list of [Chat] objects, one for each prompt.
#' For `parallel_chat_structured()`, a single structured data object with one
#' element for each prompt. Typically, when `type` is an object, this will
#' will be a data frame with one row for each prompt, and one column for each
#' property.
#' @export
#' @examplesIf ellmer::has_credentials("openai")
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
  needs_wrapper <- S7_inherits(provider, ProviderOpenAI)

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

multi_convert <- function(
  provider,
  turns,
  type,
  convert = TRUE,
  include_tokens = FALSE,
  include_cost = FALSE
) {
  needs_wrapper <- S7_inherits(provider, ProviderOpenAI)

  rows <- map(turns, \(turn) {
    extract_data(
      turn = turn,
      type = wrap_type_if_needed(type, needs_wrapper),
      convert = FALSE,
      needs_wrapper = needs_wrapper
    )
  })

  if (convert) {
    out <- convert_from_type(rows, type_array(items = type))
  } else {
    out <- rows
  }

  if (is.data.frame(out) && (include_tokens || include_cost)) {
    tokens <- t(vapply(turns, \(turn) turn@tokens, integer(2)))

    if (include_tokens) {
      out$input_tokens <- tokens[, 1]
      out$output_tokens <- tokens[, 2]
    }

    if (include_cost) {
      out$cost <- get_token_cost(
        provider@name,
        standardise_model(provider, provider@model),
        input = tokens[, 1],
        output = tokens[, 2]
      )
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
