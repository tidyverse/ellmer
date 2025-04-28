#' @include utils-coro.R
NULL

#' A chat
#'
#' @description
#' A `Chat` is a sequence of user and assistant [Turn]s sent
#' to a specific [Provider]. A `Chat` is a mutable R6 object that takes care of
#' managing the state associated with the chat; i.e. it records the messages
#' that you send to the server, and the messages that you receive back.
#' If you register a tool (i.e. an R function that the assistant can call on
#' your behalf), it also takes care of the tool loop.
#'
#' You should generally not create this object yourself,
#' but instead call [chat_openai()] or friends instead.
#'
#' @return A Chat object
#' @examplesIf has_credentials("openai")
#' chat <- chat_openai(echo = TRUE)
#' chat$chat("Tell me a funny joke")
Chat <- R6::R6Class(
  "Chat",
  public = list(
    #' @param provider A provider object.
    #' @param system_prompt System prompt to start the conversation with.
    #' @param echo One of the following options:
    #'   * `none`: don't emit any output (default when running in a function).
    #'   * `text`: echo text output as it streams in (default when running at
    #'     the console).
    #'   * `all`: echo all input and output.
    #'
    #'  Note this only affects the `chat()` method.
    initialize = function(provider, system_prompt = NULL, echo = "none") {
      private$provider <- provider
      private$echo <- echo
      self$set_system_prompt(system_prompt)
    },

    #' @description Retrieve the turns that have been sent and received so far
    #'   (optionally starting with the system prompt, if any).
    #' @param include_system_prompt Whether to include the system prompt in the
    #'   turns (if any exists).
    get_turns = function(include_system_prompt = FALSE) {
      if (length(private$.turns) == 0) {
        return(private$.turns)
      }

      if (!include_system_prompt && is_system_prompt(private$.turns[[1]])) {
        private$.turns[-1]
      } else {
        private$.turns
      }
    },

    #' @description Replace existing turns with a new list.
    #' @param value A list of [Turn]s.
    set_turns = function(value) {
      private$.turns <- normalize_turns(
        value,
        self$get_system_prompt(),
        overwrite = TRUE
      )
      invisible(self)
    },

    #' @description Add a pair of turns to the chat.
    #' @param user The user [Turn].
    #' @param system The system [Turn].
    add_turn = function(user, system) {
      check_turn(user)
      check_turn(system)

      private$.turns[[length(private$.turns) + 1]] <- user
      private$.turns[[length(private$.turns) + 1]] <- system
      invisible(self)
    },

    #' @description If set, the system prompt, it not, `NULL`.
    get_system_prompt = function() {
      if (private$has_system_prompt()) {
        private$.turns[[1]]@text
      } else {
        NULL
      }
    },

    #' @description Retrieve the model name
    get_model = function() {
      private$provider@model
    },

    #' @description Update the system prompt
    #' @param value A character vector giving the new system prompt
    set_system_prompt = function(value) {
      check_character(value, allow_null = TRUE)
      if (length(value) > 1) {
        value <- paste(value, collapse = "\n\n")
      }

      # Remove prompt, if present
      if (private$has_system_prompt()) {
        private$.turns <- private$.turns[-1]
      }
      # Add prompt, if new
      if (is.character(value)) {
        system_turn <- Turn("system", value, completed = NULL)
        private$.turns <- c(list(system_turn), private$.turns)
      }
      invisible(self)
    },

    #' @description A data frame with a `tokens` column that proides the
    #'   number of input tokens used by user turns and the number of
    #'   output tokens used by assistant turns.
    #' @param include_system_prompt Whether to include the system prompt in
    #'   the turns (if any exists).
    get_tokens = function(include_system_prompt = FALSE) {
      turns <- self$get_turns(include_system_prompt = FALSE)
      assistant_turns <- keep(turns, function(x) x@role == "assistant")

      n <- length(assistant_turns)
      tokens_acc <- t(vapply(
        assistant_turns,
        function(turn) turn@tokens,
        double(2)
      ))

      tokens <- tokens_acc
      if (n > 1) {
        # Compute just the new tokens
        tokens[-1, 1] <- tokens[seq(2, n), 1] -
          (tokens[seq(1, n - 1), 1] + tokens[seq(1, n - 1), 2])
      }
      # collapse into a single vector
      tokens_v <- c(t(tokens))
      tokens_acc_v <- c(t(tokens_acc))

      tokens_df <- data.frame(
        role = rep(c("user", "assistant"), times = n),
        tokens = tokens_v,
        tokens_total = tokens_acc_v
      )

      if (include_system_prompt && private$has_system_prompt()) {
        # How do we compute this?
        tokens_df <- rbind(
          data.frame(role = "system", tokens = 0, tokens_total = 0),
          tokens_df
        )
      }

      tokens_df
    },

    #' @description The cost of this chat
    #' @param include The default, `"all"`, gives the total cumulative cost
    #'   of this chat. Alternatively, use `"last"` to get the cost of just the
    #'   most recent turn.
    get_cost = function(include = c("all", "last")) {
      include <- arg_match(include)

      turns <- self$get_turns(include_system_prompt = FALSE)
      assistant_turns <- keep(turns, function(x) x@role == "assistant")
      n <- length(assistant_turns)
      tokens <- t(vapply(
        assistant_turns,
        function(turn) turn@tokens,
        double(2)
      ))

      if (include == "last") {
        tokens <- tokens[nrow(tokens), , drop = FALSE]
      }

      get_token_cost(
        private$provider@name,
        standardise_model(private$provider, private$provider@model),
        input = sum(tokens[, 1]),
        output = sum(tokens[, 2])
      )
    },

    #' @description The last turn returned by the assistant.
    #' @param role Optionally, specify a role to find the last turn with
    #'   for the role.
    #' @return Either a `Turn` or `NULL`, if no turns with the specified
    #'   role have occurred.
    last_turn = function(role = c("assistant", "user", "system")) {
      role <- arg_match(role)

      n <- length(private$.turns)
      switch(
        role,
        system = if (private$has_system_prompt()) private$.turns[[1]],
        assistant = if (n > 1) private$.turns[[n]],
        user = if (n > 1) private$.turns[[n - 1]]
      )
    },

    #' @description Submit input to the chatbot, and return the response as a
    #'   simple string (probably Markdown).
    #' @param ... The input to send to the chatbot. Can be strings or images
    #'   (see [content_image_file()] and [content_image_url()].
    #' @param echo Whether to emit the response to stdout as it is received. If
    #'   `NULL`, then the value of `echo` set when the chat object was created
    #'   will be used.
    chat = function(..., echo = NULL) {
      turn <- user_turn(...)
      echo <- check_echo(echo %||% private$echo)

      # Returns a single turn (the final response from the assistant), even if
      # multiple rounds of back and forth happened.
      coro::collect(private$chat_impl(
        turn,
        stream = echo != "none",
        echo = echo
      ))

      text <- self$last_turn()@text
      if (echo == "none") text else invisible(text)
    },

    #' @description `r lifecycle::badge("experimental")`
    #'
    #'   Submit multiple prompts in parallel. Returns a list of [Chat] objects,
    #'   one for each prompt.
    #' @param prompts A list of user prompts.
    #' @param max_active The maximum number of simultaenous requests to send.
    #' @param rpm Maximum number of requests per minute.
    chat_parallel = function(prompts, max_active = 10, rpm = 500) {
      turns <- as_user_turns(prompts)

      reqs <- parallel_requests(
        provider = private$provider,
        existing_turns = private$.turns,
        tools = private$tools,
        new_turns = turns,
        rpm = rpm
      )
      resps <- req_perform_parallel(reqs, max_active = max_active)
      ok <- !map_lgl(resps, is.null)
      json <- map(resps[ok], resp_body_json)

      map2(json, turns[ok], function(json, user_turn) {
        chat <- self$clone()
        turn <- value_turn(private$provider, json)
        chat$add_turn(user_turn, turn)
        chat
      })
    },

    #' @description Extract structured data
    #' @param ... The input to send to the chatbot. Will typically include
    #'   the phrase "extract structured data".
    #' @param type A type specification for the extracted data. Should be
    #'   created with a [`type_()`][type_boolean] function.
    #' @param echo Whether to emit the response to stdout as it is received.
    #'   Set to "text" to stream JSON data as it's generated (not supported by
    #'  all providers).
    #' @param convert Automatically convert from JSON lists to R data types
    #'   using the schema. For example, this will turn arrays of objects into
    #'  data frames and arrays of strings into a character vector.
    extract_data = function(..., type, echo = "none", convert = TRUE) {
      turn <- user_turn(...)
      echo <- check_echo(echo %||% private$echo)
      check_bool(convert)

      needs_wrapper <- S7_inherits(private$provider, ProviderOpenAI)
      if (needs_wrapper) {
        type <- type_object(wrapper = type)
      }

      coro::collect(private$submit_turns(
        turn,
        type = type,
        stream = echo != "none",
        echo = echo
      ))

      turn <- self$last_turn()
      extract_data(turn, type, convert = convert, needs_wrapper = needs_wrapper)
    },

    #' @description `r lifecycle::badge("experimental")`
    #'
    #'   Submit multiple prompts in parallel.
    #' @param prompts A vector created by [interpolate()] or a list
    #'   of character vectors.
    #' @param type A type specification for the extracted data. Should be
    #'   created with a [`type_()`][type_boolean] function.
    #' @param convert If `TRUE`, automatically convert from JSON lists to R
    #'   data types using the schema. This typically works best when `type` is
    #'   `type_object()` as this will give you a data frame with one column for
    #'   each property. If `FALSE`, returns a list.
    #' @param max_active The maximum number of simultaenous requests to send.
    #' @param rpm Maximum number of requests per minute.
    extract_data_parallel = function(
      prompts,
      type,
      convert = TRUE,
      max_active = 10,
      rpm = 500
    ) {
      turns <- as_user_turns(prompts)
      check_bool(convert)

      needs_wrapper <- S7_inherits(private$provider, ProviderOpenAI)
      if (needs_wrapper) {
        w_type <- type_object(wrapper = type)
      } else {
        w_type <- type
      }

      reqs <- parallel_requests(
        provider = private$provider,
        existing_turns = private$.turns,
        new_turns = turns,
        tools = private$tools,
        type = w_type,
        rpm = rpm
      )
      resps <- req_perform_parallel(reqs, max_active = max_active)
      ok <- !map_lgl(resps, is.null)
      json <- map(resps[ok], resp_body_json)

      rows <- map(json, function(json) {
        turn <- value_turn(private$provider, json, has_type = TRUE)
        extract_data(turn, type, convert = FALSE, needs_wrapper = needs_wrapper)
      })

      if (convert) {
        convert_from_type(rows, type_array(items = type))
      } else {
        rows
      }
    },

    #' @description Extract structured data, asynchronously. Returns a promise
    #'   that resolves to an object matching the type specification.
    #' @param ... The input to send to the chatbot. Will typically include
    #'   the phrase "extract structured data".
    #' @param type A type specification for the extracted data. Should be
    #'   created with a [`type_()`][type_boolean] function.
    #' @param echo Whether to emit the response to stdout as it is received.
    #'   Set to "text" to stream JSON data as it's generated (not supported by
    #'  all providers).
    extract_data_async = function(..., type, echo = "none") {
      turn <- user_turn(...)
      echo <- check_echo(echo %||% private$echo)

      done <- coro::async_collect(private$submit_turns_async(
        turn,
        type = type,
        stream = echo != "none",
        echo = echo
      ))
      promises::then(done, function(dummy) {
        turn <- self$last_turn()
        is_json <- map_lgl(turn@contents, S7_inherits, ContentJson)
        n <- sum(is_json)
        if (n != 1) {
          cli::cli_abort("Data extraction failed: {n} data results recieved.")
        }

        json <- turn@contents[[which(is_json)]]
        json@value
      })
    },

    #' @description Submit input to the chatbot, and receive a promise that
    #'   resolves with the response all at once. Returns a promise that resolves
    #'   to a string (probably Markdown).
    #' @param ... The input to send to the chatbot. Can be strings or images.
    chat_async = function(...) {
      turn <- user_turn(...)

      # Returns a single turn (the final response from the assistant), even if
      # multiple rounds of back and forth happened.
      done <- coro::async_collect(
        private$chat_impl_async(turn, stream = FALSE, echo = "none")
      )
      promises::then(done, function(dummy) {
        self$last_turn()@text
      })
    },

    #' @description Submit input to the chatbot, returning streaming results.
    #'   Returns A [coro
    #'   generator](https://coro.r-lib.org/articles/generator.html#iterating)
    #'   that yields strings. While iterating, the generator will block while
    #'   waiting for more content from the chatbot.
    #' @param ... The input to send to the chatbot. Can be strings or images.
    #' @param content Whether the stream should yield only `"text"` or `"all"`
    #'   content types created during the round. When `content = "all"`,
    #'   `stream()` yields [Content] objects.
    stream = function(..., content = c("text", "all")) {
      content <- arg_match(content)
      turn <- user_turn(...)
      private$chat_impl(turn, content = content, stream = TRUE, echo = "none")
    },

    #' @description Submit input to the chatbot, returning asynchronously
    #'   streaming results. Returns a [coro async
    #'   generator](https://coro.r-lib.org/reference/async_generator.html) that
    #'   yields string promises.
    #' @param ... The input to send to the chatbot. Can be strings or images.
    #' @param content Whether the stream should yield only `"text"` or `"all"`
    #'   content types created during the round. When `content = "all"`,
    #'   `stream()` yields [Content] objects.
    stream_async = function(..., content = c("text", "all")) {
      content <- arg_match(content)
      turn <- user_turn(...)
      private$chat_impl_async(
        turn,
        content = content,
        stream = TRUE,
        echo = "none"
      )
    },

    #' @description Register a tool (an R function) that the chatbot can use.
    #'   If the chatbot decides to use the function, ellmer will automatically
    #'   call it and submit the results back.
    #'
    #'   The return value of the function. Generally, this should either be a
    #'   string, or a JSON-serializable value. If you must have more direct
    #'   control of the structure of the JSON that's returned, you can return a
    #'   JSON-serializable value wrapped in [base::I()], which ellmer will leave
    #'   alone until the entire request is JSON-serialized.
    #' @param tool_def Tool definition created by [tool()].
    register_tool = function(tool_def) {
      if (!S7_inherits(tool_def, ToolDef)) {
        cli::cli_abort("{.arg tool} must be a <ToolDef>.")
      }

      private$tools[[tool_def@name]] <- tool_def
      invisible(self)
    },

    #' @description Get the underlying provider object. For expert use only.
    get_provider = function() {
      private$provider
    },

    #' @description Retrieve the list of registered tools.
    get_tools = function() {
      private$tools
    },

    #' @description Sets the available tools. For expert use only; most users
    #'   should use `register_tool()`.
    #'
    #' @param tools A list of tool definitions created with [ellmer::tool()].
    set_tools = function(tools) {
      if (!is_list(tools) || !all(map_lgl(tools, S7_inherits, ToolDef))) {
        msg <- "{.arg tools} must be a list of tools created with {.fn ellmer::tool}."
        if (S7_inherits(tools, ToolDef)) {
          msg <- c(msg, "i" = "Did you mean to call {.code $register_tool()}?")
        }
        cli::cli_abort(msg)
      }

      private$tools <- list()

      for (tool_def in tools) {
        self$register_tool(tool_def)
      }

      invisible(self)
    }
  ),
  private = list(
    provider = NULL,

    .turns = list(),
    echo = NULL,
    tools = list(),

    add_user_contents = function(contents) {
      stopifnot(is.list(contents))
      if (length(contents) == 0) {
        return(invisible(self))
      }

      i <- length(private$.turns)

      if (private$.turns[[i]]@role != "user") {
        private$.turns[[i + 1]] <- Turn("user", contents)
      } else {
        private$.turns[[i]]@contents <- c(
          private$.turns[[i]]@contents,
          contents
        )
      }
      invisible(self)
    },

    # If stream = TRUE, yields completion deltas. If stream = FALSE, yields
    # complete assistant turns.
    chat_impl = generator_method(function(
      self,
      private,
      user_turn,
      stream,
      echo,
      content = "text"
    ) {
      tool_errors <- list()
      withr::defer(warn_tool_errors(tool_errors))

      while (!is.null(user_turn)) {
        chunks <- private$submit_turns(
          user_turn,
          stream = stream,
          echo = echo,
          content = content
        )
        for (chunk in chunks) {
          if (content == "all") {
            if (!S7_inherits(chunk, Content)) {
              # assuming the alternative to `Content` is character
              chunk <- ContentText(chunk)
            }
          }
          yield(chunk)
        }

        user_turn <- private$invoke_tools(echo = echo)

        if (echo == "all") {
          cat(format(user_turn))
        } else if (echo == "none") {
          tool_errors <- c(tool_errors, turn_get_tool_errors(user_turn))
        }

        if (content == "all" && !is.null(user_turn)) {
          # We could just yield the user turn here or we can keep the contract
          # of only yielding `Content` objects
          # yield(user_turn)
          for (ut_content in user_turn@contents) {
            yield(ut_content)
          }
        }
      }
    }),

    # If stream = TRUE, yields completion deltas. If stream = FALSE, yields
    # complete assistant turns.
    chat_impl_async = async_generator_method(function(
      self,
      private,
      user_turn,
      stream,
      echo,
      content = "text"
    ) {
      tool_errors <- list()
      withr::defer(warn_tool_errors(tool_errors))

      while (!is.null(user_turn)) {
        chunks <- private$submit_turns_async(
          user_turn,
          stream = stream,
          echo = echo,
          content = content
        )
        for (chunk in await_each(chunks)) {
          if (content == "all") {
            if (!S7_inherits(chunk, Content)) {
              # assuming the alternative to `Content` is character
              chunk <- ContentText(chunk)
            }
          }
          yield(chunk)
        }

        user_turn <- await(private$invoke_tools_async(echo = echo))

        if (echo == "all") {
          cat(format(user_turn))
        } else if (echo == "none") {
          tool_errors <- c(tool_errors, turn_get_tool_errors(user_turn))
        }

        if (content == "all" && !is.null(user_turn)) {
          # We could just yield the user turn here or we can keep the contract
          # of only yielding `Content` objects
          # yield(user_turn)
          for (ut_content in user_turn@contents) {
            yield(ut_content)
          }
        }
      }
    }),

    # If stream = TRUE, yields completion deltas. If stream = FALSE, yields
    # complete assistant turns.
    submit_turns = generator_method(function(
      self,
      private,
      user_turn,
      stream,
      echo,
      type = NULL,
      content = "text"
    ) {
      if (echo == "all") {
        cat_line(format(user_turn), prefix = "> ")
      }

      response <- chat_perform(
        provider = private$provider,
        mode = if (stream) "stream" else "value",
        turns = c(private$.turns, list(user_turn)),
        tools = private$tools,
        type = type
      )
      emit <- emitter(echo)
      any_text <- FALSE

      if (stream) {
        result <- NULL
        for (chunk in response) {
          text <- stream_text(private$provider, chunk)
          if (!is.null(text)) {
            emit(text)
            yield(text)
            any_text <- TRUE
          }

          result <- stream_merge_chunks(private$provider, result, chunk)
        }
        turn <- value_turn(private$provider, result, has_type = !is.null(type))
        turn <- private$match_tools(turn)
      } else {
        turn <- value_turn(
          private$provider,
          response,
          has_type = !is.null(type)
        )
        turn <- private$match_tools(turn)

        text <- turn@text
        if (!is.null(text)) {
          emit(text)
          yield(text)
          any_text <- TRUE
        }
      }

      # Ensure turns always end in a newline
      if (any_text) {
        emit("\n")
        yield("\n")
      }

      is_text <- map_lgl(turn@contents, S7_inherits, ContentText)

      if (any(!is_text)) {
        contents_not_text <- turn@contents[!is_text]
        if (echo == "all") {
          formatted <- map_chr(contents_not_text, format)
          cat_line(formatted, prefix = "< ")
        }
        # When `echo="output"`, tool calls are emitted in `invoke_tools()`

        if (content == "all") {
          for (turn_content in contents_not_text) {
            yield(turn_content)
          }
          yield("\n")
        }
      }

      self$add_turn(user_turn, turn)

      coro::exhausted()
    }),

    # If stream = TRUE, yields completion deltas. If stream = FALSE, yields
    # complete assistant turns.
    submit_turns_async = async_generator_method(function(
      self,
      private,
      user_turn,
      stream,
      echo,
      type = NULL,
      content = "text"
    ) {
      response <- chat_perform(
        provider = private$provider,
        mode = if (stream) "async-stream" else "async-value",
        turns = c(private$.turns, list(user_turn)),
        tools = private$tools,
        type = type
      )
      emit <- emitter(echo)
      any_text <- FALSE

      if (stream) {
        result <- NULL
        for (chunk in await_each(response)) {
          text <- stream_text(private$provider, chunk)
          if (!is.null(text)) {
            emit(text)
            yield(text)
            any_text <- TRUE
          }

          result <- stream_merge_chunks(private$provider, result, chunk)
        }
        turn <- value_turn(private$provider, result, has_type = !is.null(type))
      } else {
        result <- await(response)

        turn <- value_turn(private$provider, result, has_type = !is.null(type))
        text <- turn@text
        if (!is.null(text)) {
          emit(text)
          yield(text)
          any_text <- TRUE
        }
      }
      turn <- private$match_tools(turn)

      # Ensure turns always end in a newline
      if (any_text) {
        emit("\n")
        yield("\n")
      }

      is_text <- map_lgl(turn@contents, S7_inherits, ContentText)
      if (!any(is_text)) {
        if (echo == "all") {
          formatted <- map_chr(turn@contents[!is_text], format)
          cat_line(formatted, prefix = "< ")
        }
        # When `echo="output"`, tool calls are echoed via `invoke_tools_async()`

        if (content == "all") {
          for (turn_content in turn@contents[!is_text]) {
            yield(turn_content)
          }
          yield("\n")
        }
      }

      self$add_turn(user_turn, turn)
      coro::exhausted()
    }),

    match_tools = function(turn) {
      if (is.null(turn)) return(NULL)

      turn@contents <- map(turn@contents, function(content) {
        if (!S7_inherits(content, ContentToolRequest)) {
          return(content)
        }
        content@tool <- private$tools[[content@name]]
        content
      })

      turn
    },

    invoke_tools = function(echo = "none") {
      tool_results <- invoke_tools(self$last_turn(), echo = echo)
      if (length(tool_results) == 0) {
        return()
      }
      Turn("user", tool_results)
    },

    invoke_tools_async = async_method(function(self, private, echo = "none") {
      tool_results <- await(invoke_tools_async(self$last_turn(), echo = echo))
      if (length(tool_results) == 0) {
        return()
      }
      Turn("user", tool_results)
    }),

    has_system_prompt = function() {
      length(private$.turns) > 0 && private$.turns[[1]]@role == "system"
    }
  )
)

is_chat <- function(x) {
  inherits(x, "Chat")
}

#' @export
print.Chat <- function(x, ...) {
  provider <- x$get_provider()
  turns <- x$get_turns(include_system_prompt = TRUE)

  tokens <- x$get_tokens(include_system_prompt = TRUE)

  tokens_user <- sum(tokens$tokens_total[tokens$role == "user"])
  tokens_assistant <- sum(tokens$tokens_total[tokens$role == "assistant"])
  cost <- x$get_cost()

  cat(paste_c(
    "<Chat",
    c(" ", provider@name, "/", provider@model),
    c(" turns=", length(turns)),
    c(
      " tokens=",
      tokens_user,
      "/",
      tokens_assistant
    ),
    if (!is.na(cost)) c(" ", format(cost)),
    ">\n"
  ))

  for (i in seq_along(turns)) {
    turn <- turns[[i]]

    color <- switch(
      turn@role,
      user = cli::col_blue,
      assistant = cli::col_green,
      system = cli::col_br_white,
      identity
    )

    cli::cat_rule(cli::format_inline(
      "{color(turn@role)} [{tokens$tokens[[i]]}]"
    ))
    for (content in turn@contents) {
      cat_line(format(content))
    }
  }

  invisible(x)
}

method(contents_markdown, new_S3_class("Chat")) <- function(
  content,
  heading_level = 2
) {
  turns <- content$get_turns()
  if (length(turns) == 0) {
    return("")
  }

  hh <- strrep("#", heading_level)

  res <- vector("character", length(turns))
  for (i in seq_along(res)) {
    role <- turns[[i]]@role
    substr(role, 0, 1) <- toupper(substr(role, 0, 1))
    res[i] <- glue::glue("{hh} {role}\n\n{contents_markdown(turns[[i]])}")
  }

  paste(res, collapse = "\n\n")
}

extract_data <- function(turn, type, convert = TRUE, needs_wrapper = FALSE) {
  is_json <- map_lgl(turn@contents, S7_inherits, ContentJson)
  n <- sum(is_json)
  if (n != 1) {
    cli::cli_abort("Data extraction failed: {n} data results recieved.")
  }

  json <- turn@contents[[which(is_json)]]
  out <- json@value

  if (needs_wrapper) {
    out <- out$wrapper
    type <- type@properties[[1]]
  }
  if (convert) {
    out <- convert_from_type(out, type)
  }
  out
}
