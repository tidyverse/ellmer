# Getting started --------------------------------------------------------

test_that("can make simple request", {
  chat <- chat_google_gemini_test("Be as terse as possible; no punctuation")
  resp <- chat$chat("What is 1 + 1?")
  expect_match(resp, "2")
  expect_equal(unname(chat$last_turn()@tokens[1:2] > 0), c(TRUE, TRUE))
})

test_that("can make simple streaming request", {
  chat <- chat_google_gemini_test("Be as terse as possible; no punctuation")
  resp <- coro::collect(chat$stream("What is 1 + 1?"))
  expect_match(paste0(unlist(resp), collapse = ""), "2")
})

test_that("can handle errors", {
  chat <- chat_google_gemini_test(model = "doesnt-exist")
  expect_snapshot(chat$chat("Hi"), error = TRUE)
})

test_that("can list models", {
  test_models(models_google_gemini)
})

# Common provider interface -----------------------------------------------

test_that("defaults are reported", {
  expect_snapshot(. <- chat_google_gemini())
})

test_that("supports standard parameters", {
  chat_fun <- chat_google_gemini_test

  test_params_stop(chat_fun)
})

test_that("supports tool calling", {
  vcr::local_cassette("google-tool")
  chat_fun <- chat_google_gemini_test

  test_tools_simple(chat_fun)
})

test_that("can extract data", {
  chat_fun <- chat_google_gemini_test

  test_data_extraction(chat_fun)
})

test_that("can fetch web pages", {
  vcr::local_cassette("google-web-fetch")
  chat_fun <- chat_google_gemini_test
  test_tool_web_fetch(chat_fun, google_tool_web_fetch())
})

test_that("can search web pages", {
  vcr::local_cassette("google-web-search")
  chat_fun <- chat_google_gemini_test
  test_tool_web_search(chat_fun, google_tool_web_search())
})

test_that("can use images", {
  vcr::local_cassette("google-image")
  chat_fun <- chat_google_gemini_test

  test_images_inline(chat_fun)
  test_images_remote_error(chat_fun)
})

test_that("can use pdfs", {
  chat_fun <- chat_google_gemini_test

  test_pdf_local(chat_fun)
})

test_that("can match prices for some common models", {
  provider <- chat_google_gemini_test()$get_provider()

  expect_true(has_cost(provider, "gemini-2.5-flash"))

  expect_false(has_cost(provider, "gemini-1.0-pro-latest"))
})

# custom behaviour -------------------------------------------------------------

test_that("vertex generates expected base_url", {
  chat <- chat_google_vertex("{location}", "{project}")

  service_endpoint <- "https://{location}-aiplatform.googleapis.com/v1"
  model <- "/projects/{project}/locations/{location}/publishers/google/"
  expect_equal(chat$get_provider()@base_url, paste0(service_endpoint, model))
})

test_that("can merge text output", {
  # output from "tell me a joke" with text changed
  messages <- c(
    '{"candidates": [{"content": {"parts": [{"text": "a"}],"role": "model"}}],"usageMetadata": {"promptTokenCount": 5,"totalTokenCount": 5},"modelVersion": "gemini-1.5-flash"}',
    '{"candidates": [{"content": {"parts": [{"text": "b"}],"role": "model"}}],"usageMetadata": {"promptTokenCount": 5,"totalTokenCount": 5},"modelVersion": "gemini-1.5-flash"}',
    '{"candidates": [{"content": {"parts": [{"text": "c"}],"role": "model"},"finishReason": "STOP"}],"usageMetadata": {"promptTokenCount": 5,"candidatesTokenCount": 17,"totalTokenCount": 22},"modelVersion": "gemini-1.5-flash"}'
  )
  chunks <- lapply(messages, jsonlite::parse_json)

  out <- merge_gemini_chunks(chunks[[1]], chunks[[2]])
  out <- merge_gemini_chunks(out, chunks[[3]])

  expect_equal(out$candidates[[1]]$content$parts[[1]]$text, "abc")
  expect_equal(
    out$usageMetadata,
    list(
      promptTokenCount = 5,
      candidatesTokenCount = 17,
      totalTokenCount = 22
    )
  )
  expect_equal(out$candidates[[1]]$finishReason, "STOP")
})

test_that("can handle citations", {
  # based on "Write me a 5-paragraph essay on the history of the tidyverse."
  messages <- c(
    '{"candidates": [{"content": {"parts": [{"text": "a"}]}, "role": "model"}]}',
    '{"candidates": [{
      "content": {"parts": [{"text": "a"}]},
      "role": "model",
      "citationMetadata": {
        "citationSources": [
          {
            "startIndex": 1,
            "endIndex": 2,
            "uri": "https://example.com",
            "license": ""
          }
        ]
      }
    }]}'
  )
  chunks <- lapply(messages, jsonlite::parse_json)

  out <- merge_gemini_chunks(chunks[[1]], chunks[[2]])
  source <- out$candidates[[1]]$citationMetadata$citationSources[[1]]
  expect_equal(source$startIndex, 1)
  expect_equal(source$endIndex, 2)
  expect_equal(source$uri, "https://example.com")
  expect_equal(source$license, "")
})

test_that("can generate images", {
  vcr::local_cassette("google-image-gen")

  chat <- chat_google_gemini_test(model = "gemini-2.5-flash-image")
  chat$chat("Draw a cat")

  turn <- chat$get_turns()[[2]]
  expect_s7_class(turn@contents[[1]], ContentImageInline)
})

test_that("can use thinking levels", {
  vcr::local_cassette("google-thinking-level")

  chat <- chat_google_gemini_test(
    model = "gemini-3.5-flash",
    params = params(temperature = 0, reasoning_effort = "low")
  )
  resp <- chat$chat("What is 1 + 1?", echo = FALSE)

  contents <- chat$last_turn()@contents
  thinking <- Filter(\(x) S7::S7_inherits(x, ContentThinking), contents)
  expect_length(thinking, 1)
  expect_gt(nchar(thinking[[1]]@thinking), 0)
  expect_match(resp, "2")
})

test_that("batch chat works", {
  chat <- chat_google_gemini_test(
    system_prompt = "Answer with just the city name"
  )

  prompts <- list(
    "What's the capital of Iowa?",
    "What's the capital of New York?",
    "What's the capital of California?",
    "What's the capital of Texas?"
  )

  out <- batch_chat_text(
    chat,
    prompts,
    path = test_path("batch/state-capitals-gemini.json")
  )
  expect_equal(out, c("Des Moines", "Albany", "Sacramento", "Austin"))
})

test_that("gemini_prepare_batch_body handles API quirks", {
  provider <- chat_google_gemini_test()$get_provider()

  body <- chat_body(
    provider,
    stream = FALSE,
    turns = list(Turn("user", "hi")),
    type = type_object(firstName = type_string())
  )
  result <- gemini_prepare_batch_body(body)

  # Batch JSONL parser requires snake_case (HTTP 400 with camelCase)
  expect_true("generation_config" %in% names(result))
  expect_null(result$generationConfig)

  # Batch JSONL parser uses response_json_schema, not response_schema;
  # schema property names like "firstName" must survive snake_case conversion
  expect_true(
    "firstName" %in%
      names(result$generation_config$response_json_schema$properties)
  )
  expect_null(result$generation_config$response_schema)

  # Batch JSONL parser rejects empty system instruction text
  body$systemInstruction <- list(parts = list(text = ""))
  expect_null(gemini_prepare_batch_body(body)$system_instruction)

  body$systemInstruction <- list(parts = list(text = "Be helpful."))
  expect_equal(
    gemini_prepare_batch_body(body)$system_instruction$parts$text,
    "Be helpful."
  )
})

test_that("batch_status waits for responsesFile after SUCCEEDED", {
  provider <- chat_google_gemini_test()$get_provider()

  returned_batch <- list(
    metadata = list(
      state = "BATCH_STATE_SUCCEEDED",
      batchStats = list(requestCount = 2L, successfulRequestCount = 2L)
    )
  )

  no_file <- batch_status(provider, returned_batch)
  expect_true(no_file$working)

  returned_batch$response = list(responsesFile = "files/abc123")

  with_file <- batch_status(
    provider,
    returned_batch
  )
  expect_false(with_file$working)
})
