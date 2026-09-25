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
