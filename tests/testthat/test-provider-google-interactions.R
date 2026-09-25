test_that("only the Gemini API supports batch requests", {
  gemini <- chat_google_gemini_test()$get_provider()
  vertex <- ProviderGoogle(
    name = "Google/Vertex",
    base_url = "https://aiplatform.googleapis.com/v1/",
    credentials = function() list()
  )
  expect_equal(has_batch_support(gemini), TRUE)
  expect_equal(has_batch_support(vertex), FALSE)
})

# ellmer -> Interactions -------------------------------------------------------

test_that("chat_body() opts out of server-side storage", {
  chat <- chat_google_gemini_test(params = params(reasoning_effort = "low"))
  body <- chat_body(
    chat$get_provider(),
    chat$get_model_object(),
    turns = list(UserTurn("hi"))
  )
  expect_equal(body$store, FALSE)
  expect_equal(body$generation_config$thinking_summaries, "auto")
})

test_that("turns become steps", {
  provider <- chat_google_gemini_test()$get_provider()
  request <- ContentToolRequest(
    "call_1",
    "get_weather",
    list(location = "Boston")
  )
  thought <- ContentThinking(
    "",
    extra = list(type = "thought", signature = "sig")
  )

  expect_equal(
    as_json(provider, AssistantTurn(list(thought, ContentText("hi"), request))),
    list(
      list(type = "thought", signature = "sig"),
      list(
        type = "model_output",
        content = list(list(type = "text", text = "hi"))
      ),
      list(
        type = "function_call",
        id = "call_1",
        name = "get_weather",
        arguments = list(location = "Boston")
      )
    )
  )
  expect_equal(
    as_json(
      provider,
      UserTurn(list(ContentToolResult("52F", request = request)))
    ),
    list(
      list(
        type = "function_result",
        call_id = "call_1",
        name = "get_weather",
        result = "52F",
        is_error = FALSE
      )
    )
  )

  # Empty arguments must serialize as {} rather than []
  empty <- as_json(provider, ContentToolRequest("call_2", "now", list()))
  expect_named(empty$arguments, character())
})

test_that("content is serialized as Interactions blocks", {
  provider <- chat_google_gemini_test()$get_provider()
  expect_equal(
    as_json(provider, ContentImageRemote("https://example.com/x.PNG?v=1#top")),
    list(
      type = "image",
      uri = "https://example.com/x.PNG?v=1#top",
      mime_type = "image/png"
    )
  )
  expect_snapshot(
    as_json(provider, ContentImageRemote("https://example.com/image")),
    error = TRUE
  )
  expect_equal(
    as_json(provider, ContentPDF("application/pdf", "YQ==", "a.pdf")),
    list(type = "document", data = "YQ==", mime_type = "application/pdf")
  )
  expect_equal(
    as_json(provider, ContentUploaded("files/abc", "video/mp4")),
    list(type = "video", uri = "files/abc", mime_type = "video/mp4")
  )

  step <- list(type = "google_search_call", id = "search_1", signature = "sig")
  expect_equal(
    as_json(provider, ContentToolRequestSearch("q", extra = step)),
    step
  )
  expect_null(as_json(provider, ContentToolRequestSearch("q")))
})

# Errors -----------------------------------------------------------------------

test_that("quota errors are not retried", {
  resp <- function(status, code) {
    response_json(
      status,
      body = list(error = list(code = code, message = "Quota exceeded"))
    )
  }
  expect_equal(gemini_is_transient(resp(429, "rate_limit_exceeded")), TRUE)
  expect_equal(gemini_is_transient(resp(429, "quota_exceeded")), FALSE)
  expect_equal(gemini_is_transient(resp(503, "service_unavailable")), TRUE)
  expect_equal(gemini_is_transient(resp(400, "invalid_request")), FALSE)
  expect_equal(
    gemini_error_body(resp(429, "quota_exceeded")),
    "Quota exceeded [quota_exceeded]"
  )

  policies <- base_request(chat_google_gemini_test()$get_provider())$policies
  expect_equal(policies$retry_is_transient, gemini_is_transient)
  expect_equal(policies$error_body, gemini_error_body)
})

# Interactions -> ellmer -------------------------------------------------------

test_that("value_turn() converts steps to contents", {
  provider <- chat_google_gemini_test()$get_provider()
  thought <- list(
    type = "thought",
    signature = "sig",
    summary = list(list(type = "text", text = "Thinking"))
  )
  # Indices are bytes, and start_index is omitted when zero
  annotation <- list(
    type = "url_citation",
    url = "https://example.com",
    title = "Example",
    end_index = 6
  )
  result <- list(
    status = "requires_action",
    usage = list(
      total_tokens = 30,
      total_input_tokens = 10,
      total_cached_tokens = 4,
      total_output_tokens = 5,
      total_thought_tokens = 15
    ),
    steps = list(
      thought,
      list(
        type = "model_output",
        content = list(
          list(
            type = "text",
            text = "Héllo world",
            annotations = list(annotation)
          )
        )
      ),
      list(
        type = "function_call",
        id = "call_1",
        name = "get_weather",
        arguments = list(location = "Boston")
      )
    )
  )

  turn <- value_turn(provider, test_model(), result)
  contents <- turn@contents
  expect_s7_class(contents[[1]], ContentThinking)
  expect_equal(contents[[1]]@thinking, "Thinking")
  expect_equal(contents[[1]]@extra, thought)
  expect_s7_class(contents[[2]], ContentText)
  expect_equal(contents[[2]]@text, "Héllo world")
  expect_s7_class(contents[[3]], ContentCitation)
  expect_equal(contents[[3]]@grounded_span, "Héllo")
  expect_equal(contents[[3]]@source@url, "https://example.com")
  expect_s7_class(contents[[4]], ContentToolRequest)
  expect_equal(contents[[4]]@id, "call_1")
  expect_equal(contents[[4]]@arguments, list(location = "Boston"))

  expect_equal(unname(turn@tokens), c(6, 20, 4))
  expect_equal(turn@finish_reason, "tool_use")

  json <- value_turn(provider, test_model(), result, has_type = TRUE)
  expect_s7_class(json@contents[[2]], ContentJson)
})

test_that("value_turn() preserves Google web metadata", {
  provider <- chat_google_gemini_test()$get_provider()
  search_call <- list(
    type = "google_search_call",
    id = "search_1",
    signature = "sig",
    arguments = list(queries = list("ellmer citations"))
  )
  search_result <- list(
    type = "google_search_result",
    call_id = "search_1",
    result = list(list(search_suggestions = "<div>...</div>"))
  )
  fetch_call <- list(
    type = "url_context_call",
    id = "fetch_1",
    arguments = list(urls = list("https://fetch.example"))
  )
  fetch_result <- list(
    type = "url_context_result",
    call_id = "fetch_1",
    result = list(list(url = "https://fetch.example", status = "success"))
  )
  annotation <- list(
    type = "url_citation",
    url = "https://example.com",
    title = "Example",
    start_index = 0,
    end_index = 8
  )
  result <- list(
    status = "completed",
    usage = list(),
    steps = list(
      search_call,
      search_result,
      fetch_call,
      fetch_result,
      list(
        type = "model_output",
        content = list(
          list(
            type = "text",
            text = "Grounded answer",
            annotations = list(annotation)
          )
        )
      )
    )
  )

  contents <- value_turn(provider, test_model(), result)@contents
  expect_s7_class(contents[[1]], ContentToolRequestSearch)
  expect_equal(contents[[1]]@query, "ellmer citations")
  expect_equal(contents[[1]]@extra, search_call)
  expect_s7_class(contents[[2]], ContentToolResponseSearch)
  expect_equal(contents[[2]]@sources[[1]]@url, "https://example.com")
  expect_s7_class(contents[[3]], ContentToolRequestFetch)
  expect_equal(contents[[3]]@url, "https://fetch.example")
  expect_s7_class(contents[[4]], ContentToolResponseFetch)
  expect_equal(contents[[4]]@status, "success")
  expect_s7_class(contents[[5]], ContentText)
  expect_s7_class(contents[[6]], ContentCitation)
  expect_equal(contents[[6]]@grounded_span, "Grounded")
})

test_that("value_turn() handles a response with no steps", {
  provider <- chat_google_gemini_test()$get_provider()
  turn <- value_turn(provider, test_model(), list(status = "incomplete"))
  expect_equal(turn@contents, list())
  expect_equal(turn@finish_reason, "max_tokens")
})

test_that("value_finish_reason() maps interaction status", {
  provider <- chat_google_gemini_test()$get_provider()
  expect_equal(
    value_finish_reason(provider, list(status = "completed")),
    "success"
  )
  expect_equal(
    value_finish_reason(provider, list(status = "incomplete")),
    "max_tokens"
  )
  expect_equal(value_finish_reason(provider, list()), NA_character_)
})

# Streaming --------------------------------------------------------------------

stream_event <- function(type, ...) {
  list(event_type = type, ...)
}

test_that("stream_merge_chunks() rebuilds the interaction from events", {
  provider <- chat_google_gemini_test()$get_provider()
  usage <- list(total_tokens = 30, total_input_tokens = 10)
  events <- list(
    stream_event(
      "interaction.created",
      interaction = list(status = "in_progress")
    ),
    stream_event("step.start", index = 0, step = list(type = "thought")),
    stream_event(
      "step.delta",
      index = 0,
      delta = list(type = "thought_signature", signature = "sig")
    ),
    stream_event("step.stop", index = 0),
    stream_event(
      "step.start",
      index = 1,
      step = list(
        type = "function_call",
        id = "call_1",
        name = "f",
        arguments = list()
      )
    ),
    stream_event(
      "step.delta",
      index = 1,
      delta = list(type = "arguments_delta", arguments = '{"x":')
    ),
    stream_event(
      "step.delta",
      index = 1,
      delta = list(type = "arguments_delta", arguments = "1}")
    ),
    stream_event("step.stop", index = 1),
    stream_event("step.start", index = 2, step = list(type = "model_output")),
    stream_event(
      "step.delta",
      index = 2,
      delta = list(type = "text", text = "Hel")
    ),
    stream_event(
      "step.delta",
      index = 2,
      delta = list(type = "text", text = "lo")
    ),
    stream_event(
      "step.delta",
      index = 2,
      delta = list(
        type = "text_annotation_delta",
        annotations = list(list(
          type = "url_citation",
          url = "u",
          end_index = 5
        ))
      )
    ),
    stream_event("step.stop", index = 2),
    stream_event(
      "interaction.completed",
      interaction = list(status = "completed", usage = usage)
    )
  )

  result <- NULL
  for (event in events) {
    result <- stream_merge_chunks(provider, result, event)
  }
  expect_equal(result$status, "completed")
  expect_equal(result$usage, usage)

  contents <- value_turn(provider, test_model(), result)@contents
  expect_equal(contents[[1]]@extra, list(type = "thought", signature = "sig"))
  expect_equal(contents[[2]]@arguments, list(x = 1))
  expect_equal(contents[[3]]@text, "Hello")
  expect_equal(contents[[4]]@grounded_span, "Hello")

  expect_null(stream_parse(provider, list(data = "[DONE]")))
  expect_snapshot(
    stream_merge_chunks(
      provider,
      result,
      stream_event(
        "error",
        error = list(code = "api_error", message = "Something went wrong")
      )
    ),
    error = TRUE
  )
})

test_that("stream_content() emits text as it arrives and activity on completion", {
  provider <- chat_google_gemini_test()$get_provider()
  events <- list(
    stream_event(
      "interaction.created",
      interaction = list(status = "in_progress")
    ),
    stream_event(
      "step.start",
      index = 0,
      step = list(
        type = "google_search_call",
        id = "s1",
        signature = "sig",
        arguments = list(queries = list())
      )
    ),
    stream_event(
      "step.delta",
      index = 0,
      delta = list(
        type = "google_search_call",
        arguments = list(queries = list("q"))
      )
    ),
    stream_event("step.start", index = 1, step = list(type = "thought")),
    stream_event(
      "step.delta",
      index = 1,
      delta = list(
        type = "thought_summary",
        content = list(type = "text", text = "Thinking")
      )
    ),
    stream_event(
      "step.start",
      index = 2,
      step = list(
        type = "function_call",
        id = "c1",
        name = "f",
        arguments = list()
      )
    ),
    stream_event("step.start", index = 3, step = list(type = "model_output")),
    stream_event(
      "step.delta",
      index = 3,
      delta = list(type = "text", text = "Hi")
    ),
    stream_event(
      "step.delta",
      index = 3,
      delta = list(
        type = "text_annotation_delta",
        annotations = list(list(
          type = "url_citation",
          url = "u",
          end_index = 2
        ))
      )
    ),
    stream_event(
      "interaction.completed",
      interaction = list(status = "completed")
    )
  )

  result <- NULL
  contents <- list()
  for (event in events) {
    result <- stream_merge_chunks(provider, result, event)
    contents <- c(contents, stream_content(provider, event, result))
  }
  # Tool requests are left to value_turn()
  expect_length(contents, 4)
  expect_s7_class(contents[[1]], ContentThinking)
  expect_equal(contents[[1]]@thinking, "Thinking")
  expect_s7_class(contents[[2]], ContentText)
  expect_s7_class(contents[[3]], ContentToolRequestSearch)
  expect_equal(contents[[3]]@query, "q")
  expect_s7_class(contents[[4]], ContentCitation)
})
