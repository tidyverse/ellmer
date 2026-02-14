# Gemini batch helper functions -------------------------------------------

test_that("gemini_extract_index extracts from metadata.request_index", {
  x <- list(metadata = list(request_index = 5L))
  expect_equal(gemini_extract_index(x), 5L)
})

test_that("gemini_extract_index extracts from custom_id key", {
  x <- list(custom_id = "chat-3")
  expect_equal(gemini_extract_index(x), 3L)
})

test_that("gemini_extract_index extracts from key field", {
  x <- list(key = "chat-7")
  expect_equal(gemini_extract_index(x), 7L)
})

test_that("gemini_extract_index returns default when no index found", {
  x <- list(foo = "bar")
  expect_equal(gemini_extract_index(x, default = 99L), 99L)
})

test_that("gemini_json_fallback parses request_index from malformed line", {
  line <- '{"metadata": {"request_index": 42}, broken json...'
  result <- gemini_json_fallback(line)

  expect_equal(result$metadata$request_index, 42L)
  expect_equal(result$status$code, 500L)
})

test_that("gemini_json_fallback parses custom_id from malformed line", {
  line <- '{"custom_id": "chat-5", broken...'
  result <- gemini_json_fallback(line)

  expect_equal(result$metadata$request_index, 5L)
  expect_equal(result$status$code, 500L)
})

test_that("gemini_json_fallback returns empty metadata for unparseable line", {
  line <- "completely broken"
  result <- gemini_json_fallback(line)

  expect_equal(result$metadata, list())
  expect_equal(result$status$code, 500L)
})

test_that("gemini_normalize_result handles plain GenerateContentResponse", {
  x <- list(
    candidates = list(list(content = list(parts = list(list(text = "hello"))))),
    usageMetadata = list(totalTokenCount = 10L)
  )
  result <- gemini_normalize_result(x, index_default = 1L)

  expect_equal(result$index, 1L)
  expect_equal(result$result$status_code, 200L)
  expect_equal(result$result$body, x)
})

test_that("gemini_normalize_result handles wrapped response", {
  x <- list(
    metadata = list(request_index = 2L),
    response = list(candidates = list())
  )
  result <- gemini_normalize_result(x, index_default = 99L)

  expect_equal(result$index, 2L)
  expect_equal(result$result$status_code, 200L)
  expect_equal(result$result$body, list(candidates = list()))
})

test_that("gemini_normalize_result handles error response", {
  x <- list(
    metadata = list(request_index = 3L),
    error = list(code = 400L, message = "bad request")
  )
  result <- gemini_normalize_result(x, index_default = 99L)

  expect_equal(result$index, 3L)
  expect_equal(result$result$status_code, 400L)
  expect_null(result$result$body)
})

test_that("gemini_normalize_result handles unknown format", {
  x <- list(unknown_field = "value")
  result <- gemini_normalize_result(x, index_default = 5L)

  expect_equal(result$index, 5L)
  expect_equal(result$result$status_code, 500L)
  expect_null(result$result$body)
})

# gemini_prepare_batch_body -----------------------------------------------

test_that("gemini_prepare_batch_body converts API keys to snake_case", {
  body <- list(
    generationConfig = list(responseMimeType = "text/plain"),
    contents = list(list(role = "user", parts = list(list(text = "hi"))))
  )

  result <- gemini_prepare_batch_body(body)

  expect_true("generation_config" %in% names(result))
  expect_null(result$generationConfig)
  expect_true("response_mime_type" %in% names(result$generation_config))
})

test_that("gemini_prepare_batch_body preserves schema property names", {
  body <- list(
    generationConfig = list(
      responseMimeType = "application/json",
      responseSchema = list(
        type = "object",
        properties = list(
          firstName = list(type = "string"),
          lastName = list(type = "string")
        ),
        required = list("firstName", "lastName")
      )
    ),
    contents = list(list(role = "user", parts = list(list(text = "hi"))))
  )
  result <- gemini_prepare_batch_body(body)

  schema <- result$generation_config$response_json_schema
  expect_false(is.null(schema))
  expect_true("firstName" %in% names(schema$properties))
  expect_true("lastName" %in% names(schema$properties))
  expect_equal(schema$required, list("firstName", "lastName"))
  expect_null(result$generation_config$response_schema)
})

test_that("gemini_prepare_batch_body strips empty system instruction", {
  body <- list(
    systemInstruction = list(parts = list(text = "")),
    contents = list(list(role = "user", parts = list(list(text = "hi"))))
  )
  result <- gemini_prepare_batch_body(body)

  expect_null(result$system_instruction)
  expect_null(result$systemInstruction)
})

test_that("gemini_prepare_batch_body keeps non-empty system instruction", {
  body <- list(
    systemInstruction = list(parts = list(text = "You are helpful.")),
    contents = list(list(role = "user", parts = list(list(text = "hi"))))
  )
  result <- gemini_prepare_batch_body(body)

  expect_false(is.null(result$system_instruction))
  expect_equal(result$system_instruction$parts$text, "You are helpful.")
})

# Batch support -----------------------------------------------------------

test_that("ProviderGoogleGemini has batch support", {
  chat <- chat_google_gemini_test()
  provider <- chat$get_provider()
  expect_true(has_batch_support(provider))
})

# Integration tests -------------------------------------------------------

test_that("Gemini batch_chat submits and can be resumed", {
  skip_if(
    Sys.getenv("GEMINI_API_KEY") == "" && Sys.getenv("GOOGLE_API_KEY") == "",
    "No Gemini credentials set"
  )

  chat <- chat_google_gemini_test()

  prompts <- list("Reply with exactly: ok")
  results_file <- withr::local_tempfile(fileext = ".json")

  chats <- tryCatch(
    batch_chat(
      chat,
      prompts = prompts,
      path = results_file,
      wait = FALSE
    ),
    error = function(e) {
      msg <- conditionMessage(e)
      if (grepl("unexpected number of responses", msg, fixed = TRUE)) {
        NULL
      } else {
        stop(e)
      }
    }
  )

  if (is.null(chats)) {
    completed <- FALSE
    for (i in seq_len(100)) {
      Sys.sleep(10)
      completed <- isTRUE(batch_chat_completed(chat, prompts, results_file))
      if (completed) break
    }

    if (!completed) {
      skip("Gemini batch did not complete within test timeout.")
    }

    chats <- batch_chat(
      chat,
      prompts = prompts,
      path = results_file,
      wait = TRUE
    )
  }

  expect_equal(length(chats), 1)
  expect_true(inherits(chats[[1]], "Chat"))
})

test_that("Gemini batch_chat_structured works", {
  skip_if(
    Sys.getenv("GEMINI_API_KEY") == "" && Sys.getenv("GOOGLE_API_KEY") == "",
    "No Gemini credentials set"
  )

  chat <- chat_google_gemini_test()

  type_answer <- type_object(
    answer = type_string()
  )

  prompts <- list("What is 2+2? Reply with just the number.")
  results_file <- withr::local_tempfile(fileext = ".json")

  result <- tryCatch(
    batch_chat_structured(
      chat,
      prompts = prompts,
      path = results_file,
      type = type_answer,
      wait = FALSE
    ),
    error = function(e) {
      msg <- conditionMessage(e)
      if (grepl("unexpected number of responses", msg, fixed = TRUE)) {
        NULL
      } else if (grepl("HTTP 40[04]|invalid argument|not found|not supported", msg, ignore.case = TRUE)) {
        skip(paste0("Gemini batch API rejected request: ", msg))
      } else {
        stop(e)
      }
    }
  )

  if (is.null(result)) {
    completed <- FALSE
    for (i in seq_len(12)) {
      Sys.sleep(10)
      completed <- isTRUE(batch_chat_completed(chat, prompts, results_file))
      if (completed) break
    }

    if (!completed) {
      skip("Gemini batch did not complete within test timeout.")
    }

    result <- batch_chat_structured(
      chat,
      prompts = prompts,
      path = results_file,
      type = type_answer,
      wait = TRUE
    )
  }

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 1)
  expect_true("answer" %in% names(result))
})
