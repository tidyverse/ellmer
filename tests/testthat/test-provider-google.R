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
  expect_true(has_cost(provider, "gemini-2.5-flash-preview-05-20"))

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
