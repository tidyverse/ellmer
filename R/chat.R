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
#' @export
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
    #' @param model A [Model] object.
    #' @param system_prompt System prompt to start the conversation with.
    #' @param echo One of the following options:
    #'   * `none`: don't emit any output (default when running in a function).
    #'   * `output`: echo text and tool-calling output after the turn completes
    #'     (default when running at the console).
    #'   * `all`: echo all input and output.
    #'
    #' Console display occurs after a turn completes so ellmer can add citation
    #' markers and a source list to the response.
    #'
    #'  Note this only affects the `chat()` method. You can override the default
    #'  by setting the `ellmer_echo` option.
    initialize = function(
      provider,
      model = NULL,
      system_prompt = NULL,
      echo = "none"
    ) {
      # Fall back to the model set via deprecated Provider properties.
      # Remove along with the deprecated properties (#1098).
      model <- model %||% provider_model(provider)
      if (is.null(model)) {
        cli::cli_abort("{.arg model} is required.")
      }
      private$provider <- provider
      private$model <- model
      provider_model(private$provider) <- model
      private$echo <- echo
      private$callback_on_tool_request <- CallbackManager$new(args = "request")
      private$callback_on_tool_result <- CallbackManager$new(args = "result")
      private$callback_on_request_start <- CallbackManager$new(args = "turns")
      private$callback_on_request_end <- CallbackManager$new(args = "turn")
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

    #' @description Retrieve the conversation grouped into [Round]s. Each
    #'   `Round` pairs a user turn with the assistant and tool-result turns it
    #'   produced.
    #' @param include_system_prompt Whether to include system turns in the
    #'   rounds. When `FALSE` (the default), all system turns are dropped. When
    #'   `TRUE`, each system turn is folded into the `input` of the round it
    #'   precedes.
    get_rounds = function(include_system_prompt = FALSE) {
      turns <- self$get_turns(include_system_prompt = TRUE)
      if (!include_system_prompt) {
        turns <- discard(turns, is_system_turn)
      }
      get_rounds(turns)
    },

    #' @description The last [Round] of conversation. Note that system prompt
    #'   turns are included, equivalent to the last item in the list of rounds
    #'   returned by `$get_rounds(include_system_prompt = TRUE)`.
    #' @return Either a `Round` or `NULL`, if no rounds have occurred.
    last_round = function() {
      rounds <- self$get_rounds(include_system_prompt = TRUE)
      if (length(rounds) == 0) NULL else rounds[[length(rounds)]]
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
        log_turn(private$provider, private$model, assistant)
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

    #' @description Retrieve the model name.
    get_model = function() {
      private$model@name
    },

    #' @description Retrieve the Model object. For expert use only.
    get_model_object = function() {
      private$model
    },

    #' @description Update the model name. Note that unlike some of the
    #'   `chat_*()` functions, the model name is not validated against available
    #'   models for the provider.
    #' @param model A single string giving the new model name.
    set_model = function(model) {
      check_string(model)
      private$model@name <- model
      provider_model(private$provider) <- private$model
      invisible(self)
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
      complete_turns <- discard(assistant_turns, is_partial_turn)
      tokens <- map_tokens(complete_turns, \(turn) turn@tokens)
      tokens <- tibble::as_tibble(tokens)
      tokens$cost <- dollars(map_dbl(complete_turns, \(turn) turn@cost))

      user_turns <- keep(turns, is_user_turn)
      tokens$input_preview <- map_chr(user_turns, turn_contents_preview)
      tokens
    },

    #' @description The cost of this chat
    #' @param include The default, `"all"`, gives the total cumulative cost
    #'   of this chat. Alternatively, use `"last"` to get the cost of just the
    #'   most recent turn. Incomplete turns (from cancelled or interrupted
    #'   streams) are excluded because they lack token data.
    get_cost = function(include = c("all", "last")) {
      include <- arg_match(include)

      turns <- self$get_turns()
      assistant_turns <- keep(turns, is_assistant_turn)
      complete_turns <- discard(assistant_turns, is_partial_turn)

      if (length(complete_turns) == 0) {
        return(dollars(0))
      }

      if (include == "last") {
        cost <- complete_turns[[length(complete_turns)]]@cost
      } else {
        cost <- sum(map_dbl(complete_turns, \(turn) turn@cost))
      }

      dollars(cost)
    },

    #' @description Estimate the token count for `...` using the
    #'   provider's token counting endpoint.
    #' @param ... Input to count tokens for.
    #' @param include What to include in the count. `"new"` counts
    #'   tokens only for the contents of `...`. `"complete"` estimates
    #'   the total input tokens for the next request, including system
    #'   prompt, tools, and conversation history.
    #' @param type An optional type specification for structured data
    #'   extraction, created with a [`type_()`][type_boolean] function.
    #' @return The estimated number of input tokens.
    token_count = function(..., include = c("new", "complete"), type = NULL) {
      include <- arg_match(include)

      if (include == "new") {
        return(count_tokens(private$provider, private$model, ..., type = type))
      }

      # With no history, we need to explicitly include system prompt and
      # tools. Otherwise, the last turn's token counts already cover them.
      tokens <- self$get_tokens()
      if (nrow(tokens) == 0) {
        all_tokens <- count_tokens(
          private$provider,
          private$model,
          ...,
          system_prompt = self$get_system_prompt(),
          tools = private$tools,
          type = type
        )
        return(all_tokens)
      }

      new_tokens <- count_tokens(
        private$provider,
        private$model,
        ...,
        type = type
      )
      last <- tokens[nrow(tokens), ]
      new_tokens + last$input + last$output + last$cached_input
    },

    #' @description
    #' `r lifecycle::badge("experimental")`
    #'
    #' Upload a file to the chat's provider, once, so later turns can
    #' reference it by id instead of re-sending its contents. Prefer this
    #' over [content_pdf_file()], [content_image_file()], or
    #' [content_document_file()] when a file is large or used across many
    #' turns. Otherwise, sending the file inline is simpler: it isn't limited
    #' to providers with a files API, and there's nothing stored on the
    #' provider's side to expire or clean up.
    #'
    #' File management is supported by [chat_openai()], [chat_anthropic()],
    #' and [chat_google_gemini()]; other providers error. Provider notes:
    #'
    #' * Gemini files always expire after 48 hours (so `expires_in_h` can't be
    #'   changed), and uploading waits until Gemini finishes processing the
    #'   file (which can take a while for large video/audio), so the returned
    #'   reference is always ready to use. The Files API isn't available on
    #'   Vertex AI; there, upload the file to a Cloud Storage bucket and
    #'   reference it with
    #'   `ContentUploaded(uri = "gs://bucket/object", mime_type = ...)`.
    #' * An OpenAI upload can also be referenced from a
    #'   [chat_openai_compatible()] chat pointed at OpenAI's Chat Completions
    #'   API, except for images, which that API can't reference by id.
    #' @param path Path to a file to upload.
    #' @param mime_type MIME type of the file. If not supplied, it's guessed
    #'   from the file extension.
    #' @param expires_in_h Number of hours until the provider deletes the
    #'   file. Defaults to 48. Anthropic accepts 1 to 2160 (90 days), OpenAI
    #'   1 to 720 (30 days), and both accept `Inf` to keep the file until you
    #'   delete it yourself. Gemini always uses 48 and can't be changed.
    #' @return A [ContentUploaded] that can be passed to `$chat()` and
    #'   friends in place of the file itself.
    file_upload = function(
      path,
      mime_type = NULL,
      expires_in_h = 48
    ) {
      file_upload(
        private$provider,
        path,
        mime_type = mime_type,
        expires_in_h = expires_in_h
      )
    },

    #' @description
    #' `r lifecycle::badge("experimental")`
    #'
    #' List files previously uploaded to the chat's provider.
    #' @return A data frame with one row per file: normalized columns
    #'   (`id`, `filename`, `mime_type`, `size_bytes`, `created_at`,
    #'   `expires_at`) first, then any provider-specific columns.
    file_list = function() {
      file_list(private$provider)
    },

    #' @description
    #' `r lifecycle::badge("experimental")`
    #'
    #' Get a reference to a file previously uploaded to the chat's provider,
    #' e.g. to reuse an upload from an earlier session. Use `$file_list()` to
    #' find the id.
    #' @param id A file id string, or a [ContentUploaded].
    #' @return A [ContentUploaded] that can be passed to `$chat()` and
    #'   friends, with file metadata (`filename`, `size_bytes`, `created_at`,
    #'   `expires_at`, and any provider-specific fields) in its `extra`
    #'   property. OpenAI doesn't report a file's MIME type, so it's guessed
    #'   from the filename.
    file_get = function(id) {
      file_get(private$provider, id)
    },

    #' @description
    #' `r lifecycle::badge("experimental")`
    #'
    #' Download a file from the chat's provider, writing it to `path`.
    #' Note that providers only serve back model-generated files (e.g. batch
    #' outputs); files you uploaded yourself can't be re-downloaded.
    #' @param id A file id string, or a [ContentUploaded].
    #' @param path Path to write the downloaded file to.
    #' @return `path`, invisibly.
    file_download = function(id, path) {
      file_download(private$provider, id, path)
    },

    #' @description
    #' `r lifecycle::badge("experimental")`
    #'
    #' Delete a file previously uploaded to the chat's provider.
    #' @param id A file id string, or a [ContentUploaded].
    file_delete = function(id) {
      file_delete(private$provider, id)
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
      coro::collect(
        private$chat_impl(
          turn,
          stream = echo != "none",
          echo = echo,
          controller = stream_controller()
        )
      )

      text <- ellmer_output(self$last_turn()@text)
      if (echo == "none") text else invisible(text)
    },

    #' @description Extract structured data.
    #'
    #' Note: tool calling is disabled during structured data extraction. See
    #' `vignette("structured-data")` for details and workarounds.
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

      stream <- echo != "none" &&
        !uses_tool_structured_output(private$provider, private$model, type)

      coro::collect(
        private$submit_turns(
          turn,
          type = type,
          stream = stream,
          echo = echo,
          controller = stream_controller()
        )
      )

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

      stream <- echo != "none" &&
        !uses_tool_structured_output(private$provider, private$model, type)

      done <- coro::async_collect(
        private$submit_turns_async(
          turn,
          type = type,
          stream = stream,
          echo = echo,
          controller = stream_controller()
        )
      )

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
          tool_mode = tool_mode,
          controller = stream_controller()
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
    #' @param type An optional `type_()` structured-data specification. When
    #'   supplied, registered tools are suppressed and the completed assistant
    #'   turn stores a `ContentJson`. The provider constrains the response to
    #'   JSON. With `stream = "text"` (the default), structured stream chunks
    #'   are raw JSON text; with `stream = "content"`, they are [Content]
    #'   objects. Streaming structured output requires native provider support;
    #'   tool-based fallback is not supported.
    #' @param stream Whether the stream should yield only `"text"` or ellmer's
    #'   rich content types. When `stream = "content"`, `stream()` yields
    #'   [Content] objects.
    #' @param controller An optional [stream_controller()] used to cancel the
    #'   stream from outside the iteration loop.
    stream = function(
      ...,
      type = NULL,
      stream = c("text", "content"),
      controller = NULL
    ) {
      controller <- as_controller(controller)
      finish_tools <- private$complete_dangling_tool_requests()

      if (!is.null(type)) {
        needs_wrapper <- type_needs_wrapper(type, private$provider)
        type <- wrap_type_if_needed(type, needs_wrapper)
      }

      turn <- user_turn(!!!finish_tools, ...)
      stream <- arg_match(stream)
      private$chat_impl(
        turn,
        stream = TRUE,
        echo = "none",
        type = type,
        yield_as_content = stream == "content",
        controller = controller
      )
    },

    #' @description Submit input to the chatbot, returning asynchronously
    #'   streaming results. Returns a [coro async
    #'   generator](https://coro.r-lib.org/reference/async_generator.html) that
    #'   yields string promises.
    #' @param ... The input to send to the chatbot. Can be strings or images.
    #' @param type An optional `type_()` structured-data specification. When
    #'   supplied, registered tools are suppressed and the completed assistant
    #'   turn stores a `ContentJson`. The provider constrains the response to
    #'   JSON. With `stream = "text"` (the default), structured stream chunks
    #'   are raw JSON text; with `stream = "content"`, they are [Content]
    #'   objects. Streaming structured output requires native provider support;
    #'   tool-based fallback is not supported.
    #' @param tool_mode Whether tools should be invoked one-at-a-time
    #'   (`"sequential"`) or concurrently (`"concurrent"`). Sequential mode is
    #'   best for interactive applications, especially when a tool may involve
    #'   an interactive user interface. Concurrent mode is the default and is
    #'   best suited for automated scripts or non-interactive applications.
    #' @param stream Whether the stream should yield only `"text"` or ellmer's
    #'   rich content types. When `stream = "content"`, `stream()` yields
    #'   [Content] objects.
    #' @param controller An optional [stream_controller()] used to cancel the
    #'   stream from outside the iteration loop.
    stream_async = function(
      ...,
      type = NULL,
      tool_mode = c("concurrent", "sequential"),
      stream = c("text", "content"),
      controller = NULL
    ) {
      controller <- as_controller(controller)
      finish_tools <- private$complete_dangling_tool_requests()

      if (!is.null(type)) {
        needs_wrapper <- type_needs_wrapper(type, private$provider)
        type <- wrap_type_if_needed(type, needs_wrapper)
      }

      turn <- user_turn(!!!finish_tools, ...)
      tool_mode <- arg_match(tool_mode)
      stream <- arg_match(stream)
      private$chat_impl_async(
        turn,
        stream = TRUE,
        echo = "none",
        tool_mode = tool_mode,
        type = type,
        yield_as_content = stream == "content",
        controller = controller
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
    },

    #' @description Register a callback that fires before each model request,
    #'   including each round of the tool loop. Use it to inspect the outgoing
    #'   request, or to compact the conversation with `$set_turns()`.
    #'
    #'   `turns` includes the pending turn about to be sent, which `$set_turns()`
    #'   re-appends automatically. So compact with
    #'   `chat$set_turns(compact(chat$get_turns()))` rather than passing `turns`
    #'   back to `$set_turns()`, which would duplicate the pending turn.
    #'
    #' @param callback A function called with a single argument `turns`, the
    #'   list of turns about to be sent. The return value is ignored, but may be
    #'   a promise when used with `$chat_async()` or `$stream_async()`.
    #'
    #' @return A function that can be called to remove the callback.
    on_request_start = function(callback) {
      private$callback_on_request_start$add(callback)
    },

    #' @description Register a callback that fires after each model request,
    #'   before any tool calls in the response are executed. Use it to track
    #'   latency or cost per request, or to observe tool requests before they
    #'   run.
    #'
    #'   If the request is cancelled, `turn` is an [AssistantPartialTurn] with
    #'   `NA` tokens and cost. If the request errors, the callback does not fire.
    #'
    #' @param callback A function called with a single argument `turn`, the
    #'   assistant turn just returned by the model. The return value is ignored,
    #'   but may be a promise when used with `$chat_async()` or
    #'   `$stream_async()`.
    #'
    #' @return A function that can be called to remove the callback.
    on_request_end = function(callback) {
      private$callback_on_request_end$add(callback)
    }
  ),
  private = list(
    provider = NULL,
    model = NULL,

    .turns = list(),
    echo = NULL,
    .conversation_id = NULL,
    tools = list(),
    callback_on_tool_request = NULL,
    callback_on_tool_result = NULL,
    callback_on_request_start = NULL,
    callback_on_request_end = NULL,

    # If stream = TRUE, yields completion deltas. If stream = FALSE, yields
    # complete assistant turns.
    chat_impl = generator_method(function(
      self,
      private,
      user_turn,
      stream,
      echo,
      type = NULL,
      yield_as_content = FALSE,
      controller = NULL
    ) {
      if (
        !is.null(type) &&
          uses_tool_structured_output(private$provider, private$model, type)
      ) {
        cli::cli_abort(c(
          "Can't stream structured output with {private$provider@name} model {.val {private$model@name}}.",
          i = "Streaming requires native structured output, but this provider and model fall back to tool calling.",
          i = "Use `$chat_structured()` instead."
        ))
      }

      tool_errors <- list()
      defer(warn_tool_errors(tool_errors))

      agent_span <- local_agent_otel_span(
        private$provider,
        private$model,
        activate = FALSE,
        conversation_id = private$.conversation_id
      )

      while (!is.null(user_turn)) {
        private$callback_on_request_start$invoke(c(
          private$.turns,
          list(user_turn)
        ))
        assistant_chunks <- private$submit_turns(
          user_turn,
          stream = stream,
          echo = echo,
          type = type,
          yield_as_content = yield_as_content,
          controller = controller,
          otel_span = agent_span
        )
        for (chunk in assistant_chunks) {
          yield(chunk)
        }

        assistant_turn <- self$last_turn()
        private$callback_on_request_end$invoke(assistant_turn)
        user_turn <- NULL

        # Don't invoke tools if the stream was cancelled
        if (controller$cancelled) {
          break
        }

        if (turn_has_tool_request(assistant_turn)) {
          turns <- self$get_turns(include_system_prompt = TRUE)
          tool_calls <- invoke_tools(
            assistant_turn,
            echo = echo,
            on_tool_request = private$callback_on_tool_request$invoke,
            on_tool_result = private$callback_on_tool_result$invoke,
            yield_request = yield_as_content,
            otel_span = agent_span,
            tool_context = \(request) new_tool_context(request, turns)
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
      type = NULL,
      tool_mode = "concurrent",
      yield_as_content = FALSE,
      controller = NULL
    ) {
      if (
        !is.null(type) &&
          uses_tool_structured_output(private$provider, private$model, type)
      ) {
        cli::cli_abort(c(
          "Can't stream structured output with {private$provider@name} model {.val {private$model@name}}.",
          i = "Streaming requires native structured output, but this provider and model fall back to tool calling.",
          i = "Use `$chat_structured()` instead."
        ))
      }

      tool_errors <- list()
      defer(warn_tool_errors(tool_errors))

      agent_span <- local_agent_otel_span(
        private$provider,
        private$model,
        activate = FALSE,
        conversation_id = private$.conversation_id
      )

      while (!is.null(user_turn)) {
        await(private$callback_on_request_start$invoke_async(c(
          private$.turns,
          list(user_turn)
        )))
        assistant_chunks <- private$submit_turns_async(
          user_turn,
          stream = stream,
          echo = echo,
          type = type,
          yield_as_content = yield_as_content,
          controller = controller,
          otel_span = agent_span
        )
        for (chunk in await_each(assistant_chunks)) {
          yield(chunk)
        }

        assistant_turn <- self$last_turn()
        await(private$callback_on_request_end$invoke_async(assistant_turn))
        user_turn <- NULL

        # Don't invoke tools if the stream was cancelled
        if (controller$cancelled) {
          break
        }

        if (turn_has_tool_request(assistant_turn)) {
          turns <- self$get_turns(include_system_prompt = TRUE)
          tool_calls <- invoke_tools_async(
            assistant_turn,
            echo = echo,
            on_tool_request = private$callback_on_tool_request$invoke_async,
            on_tool_result = private$callback_on_tool_result$invoke_async,
            yield_request = yield_as_content,
            otel_span = agent_span,
            tool_context = \(request) new_tool_context(request, turns)
          )
          if (tool_mode == "sequential") {
            tool_results <- list()
            for (tool_step in await_each(tool_calls)) {
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
      yield_as_content = FALSE,
      controller = NULL,
      otel_span = NULL
    ) {
      if (echo == "all") {
        cat_line(format(user_turn), prefix = "> ")
      }

      request_turns <- c(private$.turns, list(user_turn))
      otel_input <- otel_chat_input(private, user_turn)
      chat_span <- local_chat_otel_span(
        private$provider,
        private$model,
        turns = otel_input$turns,
        system_prompt = otel_input$system_prompt,
        parent = otel_span,
        conversation_id = private$.conversation_id
      )

      response <- chat_perform(
        provider = private$provider,
        model = private$model,
        mode = if (stream) "stream" else "value",
        turns = request_turns,
        tools = if (is.null(type)) private$tools,
        type = type,
        controller = controller,
        otel_span = chat_span
      )

      emit <- emitter(echo)
      any_text <- FALSE
      echo_ends_with_newline <- TRUE
      citation_sources <- list()
      turn <- NULL
      acc <- TurnAccumulator$new(
        self,
        private,
        controller,
        turns = request_turns
      )

      if (stream) {
        acc$begin_turn(user_turn)
        on.exit(acc$finalize_turn(), add = TRUE)

        result <- NULL
        for (chunk in response) {
          result <- stream_merge_chunks(private$provider, result, chunk)
          contents <- stream_content_with_turns(
            private$provider,
            chunk,
            result,
            turns = request_turns
          )
          for (content in contents) {
            text <- content_text(content)
            if (yield_as_content) {
              yield(content)
            } else if (is_stream_text_content(content)) {
              yield(text)
            }
            acc$update_turn(content)
            if (is_stream_text_content(content)) {
              any_text <- TRUE
            }
            if (S7_inherits(content, ContentText)) {
              emit(text)
              if (!identical(text, "")) {
                echo_ends_with_newline <- endsWith(text, "\n")
              }
            } else if (S7_inherits(content, ContentCitation)) {
              recorded <- record_citation_source(citation_sources, content)
              citation_sources <- recorded$sources
              if (!is.null(recorded$number)) {
                emit(paste0("[", recorded$number, "]"))
                echo_ends_with_newline <- FALSE
              }
            }
          }
        }

        record_chat_otel_span_status(chat_span, private$provider, result)
        turn <- acc$complete_turn(result, type = type)
        if (controller$cancelled) {
          turn <- self$last_turn()
        }
        record_chat_otel_span_output(chat_span, turn)
      } else {
        result <- resp_body_json(response)
        duration <- resp_timing(response)[["total"]] %||% NA_real_
        record_chat_otel_span_status(chat_span, private$provider, result)
        turn <- acc$add_turn(user_turn, result, duration, type = type)
        record_chat_otel_span_output(chat_span, turn)

        text <- turn@text
        if (!is.null(text)) {
          emit(text)
          any_text <- TRUE
          if (!identical(text, "")) {
            echo_ends_with_newline <- endsWith(text, "\n")
          }
          if (yield_as_content) {
            yield(ContentText(text))
          } else {
            yield(text)
          }
        }
        for (content in turn@contents) {
          if (S7_inherits(content, ContentCitation)) {
            recorded <- record_citation_source(citation_sources, content)
            citation_sources <- recorded$sources
            if (!is.null(recorded$number)) {
              emit(paste0("[", recorded$number, "]"))
              echo_ends_with_newline <- FALSE
            }
          }
        }
      }

      if (!is.null(turn)) {
        if (!echo_ends_with_newline) {
          emit("\n")
        }
        if (length(citation_sources) > 0) {
          echo_citation_footer(emit, citation_sources)
        }
        activity <- format_web_activity(turn@contents, echo == "all")
        if (!is.null(activity)) {
          emit("\n")
          emit(activity)
          emit("\n")
        }
        if (!is_partial_turn(turn) && any_text && is.null(type)) {
          if (!endsWith(turn@text, "\n")) {
            if (yield_as_content) {
              yield(ContentText("\n"))
            } else {
              yield("\n")
            }
          }
        }

        if (!is_partial_turn(turn) && echo == "all") {
          echo_non_text_contents(turn, exclude_citation_activity = TRUE)
        }
        # When `echo="output"`, tool calls are emitted in `invoke_tools()`
      }

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
      yield_as_content = FALSE,
      controller = NULL,
      otel_span = NULL
    ) {
      if (echo == "all") {
        cat_line(format(user_turn), prefix = "> ")
      }

      request_turns <- c(private$.turns, list(user_turn))
      otel_input <- otel_chat_input(private, user_turn)
      chat_span <- local_chat_otel_span(
        private$provider,
        private$model,
        turns = otel_input$turns,
        system_prompt = otel_input$system_prompt,
        parent = otel_span,
        conversation_id = private$.conversation_id
      )

      response <- chat_perform(
        provider = private$provider,
        model = private$model,
        mode = if (stream) "async-stream" else "async-value",
        turns = request_turns,
        tools = if (is.null(type)) private$tools,
        type = type,
        controller = controller,
        otel_span = chat_span
      )

      emit <- emitter(echo)
      any_text <- FALSE
      echo_ends_with_newline <- TRUE
      citation_sources <- list()
      turn <- NULL
      acc <- TurnAccumulator$new(
        self,
        private,
        controller,
        turns = request_turns
      )

      if (stream) {
        acc$begin_turn(user_turn)
        on.exit(acc$finalize_turn(), add = TRUE)

        result <- NULL
        for (chunk in await_each(response)) {
          result <- stream_merge_chunks(private$provider, result, chunk)
          contents <- stream_content_with_turns(
            private$provider,
            chunk,
            result,
            turns = request_turns
          )
          for (content in contents) {
            text <- content_text(content)
            if (yield_as_content) {
              yield(content)
            } else if (is_stream_text_content(content)) {
              yield(text)
            }
            acc$update_turn(content)
            if (is_stream_text_content(content)) {
              any_text <- TRUE
            }
            if (S7_inherits(content, ContentText)) {
              emit(text)
              if (!identical(text, "")) {
                echo_ends_with_newline <- endsWith(text, "\n")
              }
            } else if (S7_inherits(content, ContentCitation)) {
              recorded <- record_citation_source(citation_sources, content)
              citation_sources <- recorded$sources
              if (!is.null(recorded$number)) {
                emit(paste0("[", recorded$number, "]"))
                echo_ends_with_newline <- FALSE
              }
            }
          }
        }

        record_chat_otel_span_status(chat_span, private$provider, result)
        turn <- acc$complete_turn(result, type = type)
        if (controller$cancelled) {
          turn <- self$last_turn()
        }
        record_chat_otel_span_output(chat_span, turn)
      } else {
        response <- await(response)
        result <- resp_body_json(response)
        duration <- resp_timing(response)[["total"]] %||% NA_real_
        record_chat_otel_span_status(chat_span, private$provider, result)
        turn <- acc$add_turn(user_turn, result, duration, type = type)
        record_chat_otel_span_output(chat_span, turn)

        text <- turn@text
        if (!is.null(text)) {
          emit(text)
          any_text <- TRUE
          if (!identical(text, "")) {
            echo_ends_with_newline <- endsWith(text, "\n")
          }
          if (yield_as_content) {
            yield(ContentText(text))
          } else {
            yield(text)
          }
        }
        for (content in turn@contents) {
          if (S7_inherits(content, ContentCitation)) {
            recorded <- record_citation_source(citation_sources, content)
            citation_sources <- recorded$sources
            if (!is.null(recorded$number)) {
              emit(paste0("[", recorded$number, "]"))
              echo_ends_with_newline <- FALSE
            }
          }
        }
      }

      if (!is.null(turn)) {
        if (!echo_ends_with_newline) {
          emit("\n")
        }
        if (length(citation_sources) > 0) {
          echo_citation_footer(emit, citation_sources)
        }
        activity <- format_web_activity(turn@contents, echo == "all")
        if (!is.null(activity)) {
          emit("\n")
          emit(activity)
          emit("\n")
        }
        if (!is_partial_turn(turn) && any_text && is.null(type)) {
          if (!endsWith(turn@text, "\n")) {
            if (yield_as_content) {
              yield(ContentText("\n"))
            } else {
              yield("\n")
            }
          }
        }

        if (!is_partial_turn(turn) && echo == "all") {
          echo_non_text_contents(turn, exclude_citation_activity = TRUE)
        }
        # When `echo="output"`, tool calls are echoed via `invoke_tools_async()`
      }
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
  ),
  active = list(
    #' @field conversation_id Identifier for the current conversation. When
    #'   set, it is recorded as the `gen_ai.conversation.id` attribute on the
    #'   OpenTelemetry spans emitted for subsequent model calls. Assign `NULL`
    #'   to clear.
    #'
    #'   Developer-facing: intended for frameworks that manage conversation
    #'   history (e.g., Shiny apps). ellmer never generates an identifier on
    #'   its own.
    conversation_id = function(value) {
      if (missing(value)) {
        private$.conversation_id
      } else {
        check_string(value, allow_null = TRUE)
        private$.conversation_id <- value
      }
    }
  )
)

#' @export
print.Chat <- function(x, ...) {
  provider <- x$get_provider()
  model <- x$get_model_object()
  turns <- x$get_turns(include_system_prompt = TRUE)

  assistant_turns <- keep(turns, \(x) x@role == "assistant")
  complete_turns <- discard(assistant_turns, is_partial_turn)
  total_tokens <- colSums(map_tokens(complete_turns, \(x) x@tokens))
  total_cost <- sum(map_dbl(complete_turns, \(x) x@cost))

  cat(paste_c(
    "<Chat",
    c(" ", provider@name, "/", model@name),
    c(" turns=", length(turns)),
    turn_cost(total_tokens, total_cost, prefix = " "),
    ">\n"
  ))

  for (i in seq_along(turns)) {
    turn <- turns[[i]]
    if (is_partial_turn(turn)) {
      label <- paste0(" [", turn@reason, "]")
    } else if (turn@role == "assistant") {
      label <- turn_cost(turn@tokens, turn@cost, prefix = " [", suffix = "]")
    } else {
      label <- ""
    }

    cli::cat_rule(cli::format_inline("{color_role(turn@role)}{label}"))
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

TurnAccumulator <- R6::R6Class(
  "TurnAccumulator",
  public = list(
    chat = NULL,
    chat_private = NULL,
    provider = NULL,
    model = NULL,
    controller = NULL,
    turns = list(),
    turn_idx = NULL,
    start_time = NULL,

    initialize = function(chat, chat_private, controller, turns = list()) {
      self$chat <- chat
      self$chat_private <- chat_private
      self$provider <- chat$get_provider()
      self$model <- chat$get_model_object()
      self$controller <- controller
      self$turns <- turns
    },

    begin_turn = function(user_turn) {
      self$chat$add_turn(user_turn, AssistantPartialTurn(), log_tokens = FALSE)
      self$turn_idx <- length(self$chat_private$.turns)
      self$start_time <- proc.time()[["elapsed"]]
      invisible(self)
    },

    update_turn = function(content) {
      idx <- self$turn_idx
      turn <- self$chat_private$.turns[[idx]]
      turn@contents <- c(turn@contents, list(content))
      self$chat_private$.turns[[idx]] <- turn
      invisible(self)
    },

    complete_turn = function(result, type = NULL) {
      if (self$controller$cancelled) {
        return(invisible(self))
      }
      duration <- proc.time()[["elapsed"]] - self$start_time
      turn <- self$value_turn(result, type, duration = duration)
      self$chat_private$.turns[[self$turn_idx]] <- turn
      # log_turn() is called manually here because the streaming path
      # replaces a partial turn in-place rather than using Chat$add_turn(),
      # which handles logging automatically for the non-streaming path.
      log_turn(self$provider, self$model, turn)
      turn
    },

    finalize_turn = function() {
      idx <- self$turn_idx
      if (is.null(idx)) {
        return(invisible())
      }
      turn <- self$chat_private$.turns[[idx]]
      if (!is_partial_turn(turn)) {
        return(invisible())
      }
      turn@contents <- merge_content_text(turn@contents)
      turn@reason <- self$controller$reason %||% "interrupted"
      turn@duration <- proc.time()[["elapsed"]] - self$start_time
      self$chat_private$.turns[[idx]] <- turn
      log_turn(self$provider, self$model, turn)
    },

    add_turn = function(user_turn, result, duration = NA_real_, type = NULL) {
      turn <- self$value_turn(result, type, duration = duration)
      self$chat$add_turn(user_turn, turn)
      turn
    },

    value_turn = function(result, type, duration = NA_real_) {
      # Check before value_turn() so structured extraction errors before
      # trying to parse truncated JSON
      finish_reason <- value_finish_reason(self$provider, result)
      check_finish_reason(finish_reason, if (is.null(type)) "warn" else "error")

      turn <- value_turn_with_turns(
        self$provider,
        self$model,
        result,
        has_type = !is.null(type),
        turns = self$turns
      )
      turn@duration <- duration
      match_tools(turn, self$chat$get_tools())
    }
  )
)

echo_non_text_contents <- function(turn, exclude_citation_activity = FALSE) {
  contents <- Filter(\(x) !S7_inherits(x, ContentText), turn@contents)
  if (exclude_citation_activity) {
    contents <- Filter(
      \(x) {
        !S7_inherits(x, ContentCitation) &&
          !S7_inherits(x, ContentToolRequestSearch) &&
          !S7_inherits(x, ContentToolRequestFetch) &&
          !S7_inherits(x, ContentToolResponseSearch) &&
          !S7_inherits(x, ContentToolResponseFetch)
      },
      contents
    )
  }
  if (length(contents) == 0) {
    return(invisible())
  }
  formatted <- map_chr(contents, format)
  cat_line(formatted, prefix = "< ")
}

record_citation_source <- function(sources, citation) {
  source <- citation@source
  if (is.null(source)) {
    return(list(sources = sources, number = NULL))
  }

  key <- citation_source_key(source)
  number <- match(key, map_chr(sources, citation_source_key))
  if (is.na(number)) {
    sources <- append(sources, list(source))
    number <- length(sources)
  }

  list(sources = sources, number = number)
}

echo_citation_footer <- function(emit, sources) {
  if (length(sources) == 0) {
    return(invisible())
  }

  emit("\n")
  emit(format_citation_sources(sources))
  emit("\n")
}

citation_source_key <- function(source) {
  if (S7_inherits(source, WebSource)) {
    if (!is.null(source@url)) {
      return(paste0("url:", source@url))
    }
    if (!is.null(source@title)) {
      return(paste0("title:", source@title))
    }
  }

  paste0("source:", format(source))
}

format_citation_sources <- function(sources) {
  lines <- "Sources"
  for (i in seq_along(sources)) {
    source <- sources[[i]]
    label <- citation_source_label(source)
    url <- citation_source_url(source)
    entry <- paste0("[", i, "] ", label)
    if (!is.null(url) && !identical(label, url)) {
      entry <- paste0(entry, ": ", url)
    }
    lines <- c(lines, entry)
  }
  paste(lines, collapse = "\n")
}

citation_source_label <- function(source) {
  if (S7_inherits(source, WebSource)) {
    return(source@title %||% source@url %||% format(source))
  }

  format(source)
}

citation_source_url <- function(source) {
  if (S7_inherits(source, WebSource)) {
    source@url
  } else {
    NULL
  }
}

format_web_activity <- function(contents, include_activity) {
  if (!include_activity) {
    return(NULL)
  }

  searches <- web_activity_count(
    contents,
    ContentToolRequestSearch,
    ContentToolResponseSearch
  )
  fetches <- web_activity_count(
    contents,
    ContentToolRequestFetch,
    ContentToolResponseFetch
  )
  if (searches == 0 && fetches == 0) {
    return(NULL)
  }

  paste0(
    "Web activity: ",
    searches,
    if (searches == 1) " search" else " searches",
    ", ",
    fetches,
    if (fetches == 1) " fetch" else " fetches"
  )
}

web_activity_count <- function(contents, request_class, response_class) {
  max(
    sum(map_lgl(contents, S7_inherits, request_class)),
    sum(map_lgl(contents, S7_inherits, response_class))
  )
}

merge_content_text <- function(contents) {
  reduce(contents, .init = list(), function(acc, item) {
    n <- length(acc)
    if (n > 0 && every(list(acc[[n]], item), S7_inherits, ContentText)) {
      acc[[n]] <- ContentText(paste0(acc[[n]]@text, item@text))
    } else {
      acc <- c(acc, list(item))
    }
    acc
  })
}
method(contents_markdown, new_S3_class("Chat")) <- function(
  content,
  heading_level = 2
) {
  turns <- content$get_turns()
  if (length(turns) == 0) {
    return("")
  }

  turns_markdown(turns, heading_level)
}
