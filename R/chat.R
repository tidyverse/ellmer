#' @include utils-coro.R
NULL

#' The Chat object
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
#' @examples
#' \dontshow{ellmer:::vcr_example_start("Chat")}
#' chat <- chat_openai()
#' chat$chat("Tell me a funny joke")
#' \dontshow{ellmer:::vcr_example_end()}
Chat <- R6::R6Class(
  "Chat",
  public = list(
    #' @param provider A provider object.
    #' @param system_prompt System prompt to start the conversation with.
    #' @param echo One of the following options:
    #'   * `none`: don't emit any output (default when running in a function).
    #'   * `output`: echo text and tool-calling output as it streams in (default
    #'     when running at the console).
    #'   * `all`: echo all input and output.
    #'
    #'  Note this only affects the `chat()` method. You can override the default
    #'  by setting the `ellmer_echo` option.
    initialize = function(provider, system_prompt = NULL, echo = "none") {
      private$provider <- provider
      private$echo <- echo
      private$callback_on_tool_request <- CallbackManager$new(args = "request")
      private$callback_on_tool_result <- CallbackManager$new(args = "result")
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

      if (!include_system_prompt && is_system_turn(private$.turns[[1]])) {
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
    #' @param assistant The system [Turn].
    #' @param log_tokens Should tokens used in the turn be logged to the
    #'   session counter?
    add_turn = function(user, assistant, log_tokens = TRUE) {
      check_turn(user)
      check_turn(assistant)

      if (log_tokens) {
        log_turn(private$provider, assistant)
      }

      private$.turns[[length(private$.turns) + 1]] <- user
      private$.turns[[length(private$.turns) + 1]] <- assistant
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
        system_turn <- SystemTurn(value)
        private$.turns <- c(list(system_turn), private$.turns)
      }
      invisible(self)
    },

    #' @description A data frame with token usage and cost data. There are four
    #'   columns: `input`, `output`, `cached_input`, and `cost`. There is one
    #'   row for each assistant turn, because token counts and costs are only
    #'   available when the API returns the assistant's response.
    #' @param include_system_prompt `r lifecycle::badge("deprecated")`
    get_tokens = function(include_system_prompt = deprecated()) {
      if (lifecycle::is_present(include_system_prompt)) {
        lifecycle::deprecate_warn(
          "0.4.0",
          "get_tokens(include_system_prompt)",
          "get_tokens()"
        )
      }

      turns <- self$get_turns()
      assistant_turns <- keep(turns, is_assistant_turn)
      tokens <- map_tokens(assistant_turns, \(turn) turn@tokens)
      tokens <- tibble::as_tibble(tokens)
      tokens$cost <- dollars(map_dbl(assistant_turns, \(turn) turn@cost))

      user_turns <- keep(turns, is_user_turn)
      tokens$input_preview <- map_chr(user_turns, turn_contents_preview)
      tokens
    },

    #' @description The cost of this chat
    #' @param include The default, `"all"`, gives the total cumulative cost
    #'   of this chat. Alternatively, use `"last"` to get the cost of just the
    #'   most recent turn.
    get_cost = function(include = c("all", "last")) {
      include <- arg_match(include)

      turns <- self$get_turns()
      assistant_turns <- keep(turns, is_assistant_turn)

      if (length(assistant_turns) == 0) {
        return(dollars(0))
      }

      if (include == "last") {
        cost <- assistant_turns[[length(assistant_turns)]]@cost
      } else {
        cost <- sum(map_dbl(assistant_turns, \(turn) turn@cost))
      }

      dollars(cost)
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
      finish_tools <- private$complete_dangling_tool_requests()

      turn <- user_turn(!!!finish_tools, ...)
      echo <- check_echo(echo %||% private$echo)

      # Returns a single turn (the final response from the assistant), even if
      # multiple rounds of back and forth happened.
      coro::collect(private$chat_impl(
        turn,
        stream = echo != "none",
        echo = echo
      ))

      text <- ellmer_output(self$last_turn()@text)
      if (echo == "none") text else invisible(text)
    },

    #' @description Extract structured data
    #' @param ... The input to send to the chatbot. This is typically the text
    #'   you want to extract data from, but it can be omitted if the data is
    #'   obvious from the existing conversation.
    #' @param type A type specification for the extracted data. Should be
    #'   created with a [`type_()`][type_boolean] function.
    #' @param echo Whether to emit the response to stdout as it is received.
    #'   Set to "text" to stream JSON data as it's generated (not supported by
    #'   all providers).
    #' @param convert Automatically convert from JSON lists to R data types
    #'   using the schema. For example, this will turn arrays of objects into
    #'   data frames and arrays of strings into a character vector.
    chat_structured = function(..., type, echo = "none", convert = TRUE) {
      finish_tools <- private$complete_dangling_tool_requests()

      turn <- user_turn(!!!finish_tools, ..., .check_empty = FALSE)
      echo <- check_echo(echo %||% private$echo)
      check_bool(convert)

      needs_wrapper <- type_needs_wrapper(type, private$provider)
      type <- wrap_type_if_needed(type, needs_wrapper)

      coro::collect(private$submit_turns(
        turn,
        type = type,
        stream = echo != "none",
        echo = echo
      ))

      turn <- self$last_turn()
      extract_data(turn, type, convert = convert, needs_wrapper = needs_wrapper)
    },

    #' @description Extract structured data, asynchronously. Returns a promise
    #'   that resolves to an object matching the type specification.
    #' @param ... The input to send to the chatbot. Will typically include
    #'   the phrase "extract structured data".
    #' @param type A type specification for the extracted data. Should be
    #'   created with a [`type_()`][type_boolean] function.
    #' @param echo Whether to emit the response to stdout as it is received.
    #'   Set to "text" to stream JSON data as it's generated (not supported by
    #'   all providers).
    #' @param convert Automatically convert from JSON lists to R data types
    #'   using the schema. For example, this will turn arrays of objects into
    #'   data frames and arrays of strings into a character vector.
    chat_structured_async = function(..., type, echo = "none", convert = TRUE) {
      finish_tools <- private$complete_dangling_tool_requests()

      turn <- user_turn(!!!finish_tools, ..., .check_empty = FALSE)
      echo <- check_echo(echo %||% private$echo)
      check_bool(convert)

      needs_wrapper <- type_needs_wrapper(type, private$provider)
      type <- wrap_type_if_needed(type, needs_wrapper)

      done <- coro::async_collect(private$submit_turns_async(
        turn,
        type = type,
        stream = echo != "none",
        echo = echo
      ))

      promises::then(done, function(dummy) {
        turn <- self$last_turn()
        extract_data(
          turn,
          type,
          convert = convert,
          needs_wrapper = needs_wrapper
        )
      })
    },

    #' @description Submit input to the chatbot, and receive a promise that
    #'   resolves with the response all at once. Returns a promise that resolves
    #'   to a string (probably Markdown).
    #' @param ... The input to send to the chatbot. Can be strings or images.
    #' @param tool_mode Whether tools should be invoked one-at-a-time
    #'   (`"sequential"`) or concurrently (`"concurrent"`). Sequential mode is
    #'   best for interactive applications, especially when a tool may involve
    #'   an interactive user interface. Concurrent mode is the default and is
    #'   best suited for automated scripts or non-interactive applications.
    chat_async = function(..., tool_mode = c("concurrent", "sequential")) {
      finish_tools <- private$complete_dangling_tool_requests()

      turn <- user_turn(!!!finish_tools, ...)
      tool_mode <- arg_match(tool_mode)

      # Returns a single turn (the final response from the assistant), even if
      # multiple rounds of back and forth happened.
      done <- coro::async_collect(
        private$chat_impl_async(
          turn,
          stream = FALSE,
          echo = "none",
          tool_mode = tool_mode
        )
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
    #' @param stream Whether the stream should yield only `"text"` or ellmer's
    #'   rich content types. When `stream = "content"`, `stream()` yields
    #'   [Content] objects.
    stream = function(..., stream = c("text", "content")) {
      finish_tools <- private$complete_dangling_tool_requests()

      turn <- user_turn(!!!finish_tools, ...)
      stream <- arg_match(stream)
      private$chat_impl(
        turn,
        stream = TRUE,
        echo = "none",
        yield_as_content = stream == "content"
      )
    },

    #' @description Submit input to the chatbot, returning asynchronously
    #'   streaming results. Returns a [coro async
    #'   generator](https://coro.r-lib.org/reference/async_generator.html) that
    #'   yields string promises.
    #' @param ... The input to send to the chatbot. Can be strings or images.
    #' @param tool_mode Whether tools should be invoked one-at-a-time
    #'   (`"sequential"`) or concurrently (`"concurrent"`). Sequential mode is
    #'   best for interactive applications, especially when a tool may involve
    #'   an interactive user interface. Concurrent mode is the default and is
    #'   best suited for automated scripts or non-interactive applications.
    #' @param stream Whether the stream should yield only `"text"` or ellmer's
    #'   rich content types. When `stream = "content"`, `stream()` yields
    #'   [Content] objects.
    stream_async = function(
      ...,
      tool_mode = c("concurrent", "sequential"),
      stream = c("text", "content")
    ) {
      finish_tools <- private$complete_dangling_tool_requests()

      turn <- user_turn(!!!finish_tools, ...)
      tool_mode <- arg_match(tool_mode)
      stream <- arg_match(stream)
      private$chat_impl_async(
        turn,
        stream = TRUE,
        echo = "none",
        tool_mode = tool_mode,
        yield_as_content = stream == "content"
      )
    },

    #' @description Register a tool (an R function) that the chatbot can use.
    #'   Learn more in `vignette("tool-calling")`.
    #' @param tool A tool definition created by [tool()].
    register_tool = function(tool) {
      check_tool(tool)
      if (has_name(private$tools, tool@name)) {
        cli::cli_inform("Replacing existing {tool@name} tool.")
      }

      private$tools[[tool@name]] <- tool
      invisible(self)
    },

    #' @description Register a list of tools.
    #'   Learn more in `vignette("tool-calling")`.
    #' @param tools A list of tool definitions created by [tool()].
    register_tools = function(tools) {
      check_tools(tools)

      for (tool in tools) {
        self$register_tool(tool)
      }
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
      check_tools(tools)

      private$tools <- list()
      for (tool_def in tools) {
        self$register_tool(tool_def)
      }
      invisible(self)
    },

    #' @description Register a callback for a tool request event.
    #'
    #' @param callback A function to be called when a tool request event occurs,
    #'   which must have `request` as its only argument.
    #'
    #' @return A function that can be called to remove the callback.
    on_tool_request = function(callback) {
      private$callback_on_tool_request$add(callback)
    },

    #' @description Register a callback for a tool result event.
    #'
    #' @param callback A function to be called when a tool result event occurs,
    #'   which must have `result` as its only argument.
    #'
    #' @return A function that can be called to remove the callback.
    on_tool_result = function(callback) {
      private$callback_on_tool_result$add(callback)
    }
  ),
  private = list(
    provider = NULL,

    .turns = list(),
    echo = NULL,
    tools = list(),
    callback_on_tool_request = NULL,
    callback_on_tool_result = NULL,

    # If stream = TRUE, yields completion deltas. If stream = FALSE, yields
    # complete assistant turns.
    chat_impl = generator_method(function(
      self,
      private,
      user_turn,
      stream,
      echo,
      yield_as_content = FALSE
    ) {
      tool_errors <- list()
      withr::defer(warn_tool_errors(tool_errors))

      while (!is.null(user_turn)) {
        assistant_chunks <- private$submit_turns(
          user_turn,
          stream = stream,
          echo = echo,
          yield_as_content = yield_as_content
        )
        for (chunk in assistant_chunks) {
          yield(chunk)
        }

        assistant_turn <- self$last_turn()
        user_turn <- NULL

        if (turn_has_tool_request(assistant_turn)) {
          tool_calls <- invoke_tools(
            assistant_turn,
            echo = echo,
            on_tool_request = private$callback_on_tool_request$invoke,
            on_tool_result = private$callback_on_tool_result$invoke,
            yield_request = yield_as_content
          )

          tool_results <- list()

          for (tool_step in tool_calls) {
            if (yield_as_content) {
              yield(tool_step)
            }
            if (is_tool_result(tool_step)) {
              tool_results <- c(tool_results, list(tool_step))
            }
          }

          user_turn <- tool_results_as_turn(tool_results)
        }

        if (echo == "all") {
          cat(format(user_turn))
        } else if (echo == "none") {
          tool_errors <- c(tool_errors, turn_get_tool_errors(user_turn))
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
      tool_mode = "concurrent",
      yield_as_content = FALSE
    ) {
      tool_errors <- list()
      withr::defer(warn_tool_errors(tool_errors))

      while (!is.null(user_turn)) {
        assistant_chunks <- private$submit_turns_async(
          user_turn,
          stream = stream,
          echo = echo,
          yield_as_content = yield_as_content
        )
        for (chunk in await_each(assistant_chunks)) {
          yield(chunk)
        }

        assistant_turn <- self$last_turn()
        user_turn <- NULL

        if (turn_has_tool_request(assistant_turn)) {
          tool_calls <- invoke_tools_async(
            assistant_turn,
            echo = echo,
            on_tool_request = private$callback_on_tool_request$invoke_async,
            on_tool_result = private$callback_on_tool_result$invoke_async,
            yield_request = yield_as_content
          )
          if (tool_mode == "sequential") {
            tool_results <- list()
            for (tool_step in coro::await_each(tool_calls)) {
              if (yield_as_content) {
                yield(tool_step)
              }
              if (is_tool_result(tool_step)) {
                tool_results <- c(tool_results, list(tool_step))
              }
            }
          } else {
            tool_results <- coro::collect(tool_calls)
            if (yield_as_content) {
              # Filter out and yield tool requests before awaiting tool results
              is_request <- map_lgl(tool_results, is_tool_request)
              for (tool_step in tool_results[is_request]) {
                yield(tool_step)
              }
              tool_results <- tool_results[!is_request]
            }
            tool_results <- await(promises::promise_all(.list = tool_results))
            if (yield_as_content) {
              for (tool_result in tool_results) {
                yield(tool_result)
              }
            }
          }

          user_turn <- tool_results_as_turn(tool_results)
        }

        if (echo == "all") {
          cat(format(user_turn))
        } else if (echo == "none") {
          tool_errors <- c(tool_errors, turn_get_tool_errors(user_turn))
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
      yield_as_content = FALSE
    ) {
      if (echo == "all") {
        cat_line(format(user_turn), prefix = "> ")
      }

      response <- chat_perform(
        provider = private$provider,
        mode = if (stream) "stream" else "value",
        turns = c(private$.turns, list(user_turn)),
        tools = if (is.null(type)) private$tools,
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
            if (yield_as_content) {
              yield(ContentText(text))
            } else {
              yield(text)
            }
            any_text <- TRUE
          }

          result <- stream_merge_chunks(private$provider, result, chunk)
        }
        turn <- value_turn(private$provider, result, has_type = !is.null(type))
        turn <- match_tools(turn, private$tools)
      } else {
        turn <- value_turn(
          private$provider,
          resp_body_json(response),
          has_type = !is.null(type)
        )
        turn@duration <- resp_timing(response)[["total"]] %||% NA_real_
        turn <- match_tools(turn, private$tools)

        text <- turn@text
        if (!is.null(text)) {
          emit(text)
          if (yield_as_content) {
            yield(ContentText(text))
          } else {
            yield(text)
          }
          any_text <- TRUE
        }
      }

      # Ensure turns always end in a newline
      if (any_text) {
        emit("\n")
        if (yield_as_content) {
          yield(ContentText("\n"))
        } else {
          yield("\n")
        }
      }

      if (echo == "all") {
        is_text <- map_lgl(turn@contents, S7_inherits, ContentText)
        formatted <- map_chr(turn@contents[!is_text], format)
        cat_line(formatted, prefix = "< ")
      }
      # When `echo="output"`, tool calls are emitted in `invoke_tools()`

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
      yield_as_content = FALSE
    ) {
      response <- chat_perform(
        provider = private$provider,
        mode = if (stream) "async-stream" else "async-value",
        turns = c(private$.turns, list(user_turn)),
        tools = if (is.null(type)) private$tools,
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
            if (yield_as_content) {
              yield(ContentText(text))
            } else {
              yield(text)
            }
            any_text <- TRUE
          }

          result <- stream_merge_chunks(private$provider, result, chunk)
        }
        turn <- value_turn(private$provider, result, has_type = !is.null(type))
      } else {
        result <- await(response)

        turn <- value_turn(
          private$provider,
          resp_body_json(result),
          has_type = !is.null(type)
        )
        turn@duration <- resp_timing(result)[["total"]] %||% NA_real_
        text <- turn@text
        if (!is.null(text)) {
          emit(text)
          if (yield_as_content) {
            yield(ContentText(text))
          } else {
            yield(text)
          }
          any_text <- TRUE
        }
      }
      turn <- match_tools(turn, private$tools)

      # Ensure turns always end in a newline
      if (any_text) {
        emit("\n")
        if (yield_as_content) {
          yield(ContentText("\n"))
        } else {
          yield("\n")
        }
      }

      if (echo == "all") {
        is_text <- map_lgl(turn@contents, S7_inherits, ContentText)
        formatted <- map_chr(turn@contents[!is_text], format)
        cat_line(formatted, prefix = "< ")
      }
      # When `echo="output"`, tool calls are echoed via `invoke_tools_async()`

      self$add_turn(user_turn, turn)
      coro::exhausted()
    }),

    has_system_prompt = function() {
      length(private$.turns) > 0 && is_system_turn(private$.turns[[1]])
    },

    complete_dangling_tool_requests = function() {
      if (length(private$.turns) == 0) {
        return(NULL)
      }

      last_turn <- private$.turns[[length(private$.turns)]]
      if (last_turn@role != "assistant") {
        return(NULL)
      }

      tool_requests <- keep(last_turn@contents, is_tool_request)
      if (length(tool_requests) == 0) {
        return(NULL)
      }

      lapply(tool_requests, function(req) {
        ContentToolResult(
          error = "Chat ended before the tool could be invoked.",
          request = req
        )
      })
    }
  )
)

#' @export
print.Chat <- function(x, ...) {
  provider <- x$get_provider()
  turns <- x$get_turns(include_system_prompt = TRUE)

  assistant_turns <- keep(turns, \(x) x@role == "assistant")
  total_tokens <- colSums(map_tokens(assistant_turns, \(x) x@tokens))
  total_cost <- sum(map_dbl(assistant_turns, \(x) x@cost))

  cat(paste_c(
    "<Chat",
    c(" ", provider@name, "/", provider@model),
    c(" turns=", length(turns)),
    turn_cost(total_tokens, total_cost, prefix = " "),
    ">\n"
  ))

  for (i in seq_along(turns)) {
    turn <- turns[[i]]
    if (turn@role == "assistant") {
      cost <- turn_cost(turn@tokens, turn@cost, prefix = " [", suffix = "]")
    } else {
      cost <- ""
    }

    cli::cat_rule(cli::format_inline("{color_role(turn@role)}{cost}"))
    cat(format(turns[[i]]))
  }

  invisible(x)
}

turn_cost <- function(tokens, cost, prefix, suffix = "") {
  out <- paste0(prefix, "input=")

  if (!is.na(tokens[[3]]) && tokens[[3]] > 0) {
    out <- paste0(out, tokens[[1]], "+", tokens[[3]])
  } else {
    out <- paste0(out, tokens[[1]])
  }
  out <- paste0(out, " output=", tokens[[2]])

  if (!is.na(cost)) {
    out <- paste0(out, " cost=", format(dollars(cost)))
  }
  out <- paste0(out, suffix)
  out
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
