# Getting started --------------------------------------------------------

test_that("can make simple request", {
  chat <- chat_google_gemini_test("Be as terse as possible; no punctuation")
  resp <- chat$chat("What is 1 + 1?")
  expect_match(resp, "2")
  expect_equal(chat$last_turn()@tokens > 0, c(TRUE, TRUE))
})

test_that("can make simple streaming request", {
  chat <- chat_google_gemini_test("Be as terse as possible; no punctuation")
  resp <- coro::collect(chat$stream("What is 1 + 1?"))
  expect_match(paste0(unlist(resp), collapse = ""), "2")
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

test_that("all tool variations work", {
  chat_fun <- chat_google_gemini_test

  test_tools_simple(chat_fun)
  test_tools_async(chat_fun)
  test_tools_parallel(chat_fun)
  test_tools_sequential(chat_fun, total_calls = 6)
})

test_that("can extract data", {
  chat_fun <- chat_google_gemini_test

  test_data_extraction(chat_fun)
})

test_that("can use images", {
  chat_fun <- chat_google_gemini_test

  test_images_inline(chat_fun)
  test_images_remote_error(chat_fun)
})

test_that("can use pdfs", {
  chat_fun <- chat_google_gemini_test

  test_pdf_local(chat_fun)
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

test_that("strips suffix from model name", {
  provider <- ProviderGoogleGemini("", model = "", base_url = "", api_key = "")
  expect_equal(
    standardise_model(provider, "gemini-1.0-pro"),
    "gemini-1.0-pro"
  )
  expect_equal(
    standardise_model(provider, "gemini-1.0-pro-latest"),
    "gemini-1.0-pro"
  )
  expect_equal(
    standardise_model(provider, "gemini-1.0-pro-001"),
    "gemini-1.0-pro"
  )
  expect_equal(
    standardise_model(provider, "gemini-2.0-pro-exp-02-05"),
    "gemini-2.0-pro"
  )
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
