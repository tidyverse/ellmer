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
        result = list(list(type = "text", text = "52F")),
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
    as_json(provider, ContentImageRemote("https://example.com/x.png")),
    list(type = "image", uri = "https://example.com/x.png")
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
      body = list(error = list(code = code, message = "boom"))
    )
  }
  expect_equal(gemini_is_transient(resp(429, "rate_limit_exceeded")), TRUE)
  expect_equal(gemini_is_transient(resp(429, "quota_exceeded")), FALSE)
  expect_equal(gemini_is_transient(resp(503, "service_unavailable")), TRUE)
  expect_equal(gemini_is_transient(resp(400, "invalid_request")), FALSE)
  expect_equal(
    gemini_error_body(resp(429, "quota_exceeded")),
    "boom [quota_exceeded]"
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
  # start_index is omitted when zero
  annotation <- list(
    type = "url_citation",
    url = "https://example.com",
    title = "Example",
    end_index = 5
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
            text = "Hello world",
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
  expect_equal(contents[[2]]@text, "Hello world")
  expect_s7_class(contents[[3]], ContentCitation)
  expect_equal(contents[[3]]@grounded_span, "Hello")
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
