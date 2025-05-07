chat_parallel <- function(chat, prompts, max_active = 10, rpm = 500) {
  my_parallel_responses <- function(conversations) {
    parallel_responses(
      provider = chat$get_provider(),
      conversations = conversations,
      tools = chat$get_tools(),
      max_active = max_active,
      rpm = rpm
    )
  }

  # First build up list of cumulative
  user_turns <- as_user_turns(prompts)
  existing <- chat$get_turns(include_system_prompt = TRUE)
  conversations <- append_turns(list(existing), user_turns)

  # Now get the assistants response
  assistant_turns <- my_parallel_responses(conversations)
  conversations <- append_turns(conversations, assistant_turns)

  repeat {
    assistant_turns <- map(
      assistant_turns,
      \(turn) match_tools(turn, tools = chat$get_tools())
    )
    user_turns <- lapply(assistant_turns, invoke_tools)
    needs_iter <- !map_lgl(user_turns, is.null)
    if (!any(needs_iter)) {
      break
    }

    # don't need to index because user_turns null
    conversations <- append_turns(conversations, user_turns)

    assistant_turns <- vector("list", length(user_turns))
    assistant_turns[needs_iter] <- my_parallel_responses(
      conversations[needs_iter]
    )
    conversations <- append_turns(conversations, assistant_turns)
  }

  map(conversations, \(turns) chat$clone()$set_turns(turns))
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

parallel_responses <- function(
  provider,
  conversations,
  tools,
  max_active = 10,
  rpm = 60
) {
  reqs <- map(conversations, function(turns) {
    chat_request(
      provider = provider,
      turns = turns,
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
    value_turn(provider, json)
  })
}

parallel_requests <- function(
  provider,
  existing_turns,
  new_turns,
  tools = list(),
  type = NULL,
  rpm = 60
) {
  reqs <- map(new_turns, function(new_turn) {
    chat_request(
      provider = provider,
      turns = c(existing_turns, list(new_turn)),
      tools = tools,
      stream = FALSE,
      type = type
    )
  })

  reqs
}
