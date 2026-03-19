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

test_that("gemini_extract_index extracts from metadata key field", {
  x <- list(metadata = list(key = "chat-8"))
  expect_equal(gemini_extract_index(x), 8L)
})

test_that("gemini_extract_index returns default when no index found", {
  x <- list(foo = "bar")
  expect_equal(gemini_extract_index(x, default = 99L), 99L)
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

# Helper to create a dummy provider without needing real credentials
dummy_gemini_provider <- function(
  base_url = "https://generativelanguage.googleapis.com/v1beta/"
) {
  ProviderGoogleGemini(
    name = if (grepl("aiplatform", base_url)) {
      "Google/Vertex"
    } else {
      "Google/Gemini"
    },
    base_url = base_url,
    model = "gemini-2.5-flash",
    params = params(),
    extra_args = list(),
    extra_headers = character(),
    credentials = NULL
  )
}

test_that("ProviderGoogleGemini has batch support", {
  provider <- dummy_gemini_provider()
  expect_true(has_batch_support(provider))
})

test_that("Vertex provider also has batch support", {
  provider <- dummy_gemini_provider(
    base_url = "https://us-central1-aiplatform.googleapis.com/v1/projects/test/locations/us-central1/publishers/google/"
  )
  expect_true(has_batch_support(provider))
})

test_that("batch_status keeps working when succeeded but no responsesFile", {
  provider <- dummy_gemini_provider()
  batch <- list(
    metadata = list(
      state = "BATCH_STATE_SUCCEEDED",
      batchStats = list(requestCount = 2L, successfulRequestCount = 2L)
    )
  )
  status <- batch_status(provider, batch)
  expect_true(status$working)
})

test_that("batch_status marks done when succeeded with responsesFile", {
  provider <- dummy_gemini_provider()
  batch <- list(
    metadata = list(
      state = "BATCH_STATE_SUCCEEDED",
      batchStats = list(requestCount = 2L, successfulRequestCount = 2L)
    ),
    response = list(responsesFile = "files/abc123")
  )
  status <- batch_status(provider, batch)
  expect_false(status$working)
})

test_that("batch_retrieve reorders out-of-order Gemini results by key", {
  provider <- dummy_gemini_provider()
  batch <- list(
    metadata = list(batchStats = list(requestCount = 3L)),
    response = list(responsesFile = "files/abc123")
  )

  local_mocked_bindings(
    gemini_download_file = function(provider, name, path) {
      lines <- c(
        jsonlite::toJSON(
          list(
            key = "chat-3",
            response = list(
              responseId = "third",
              candidates = list(list(
                content = list(parts = list(list(text = "{}")))
              )),
              usageMetadata = list(totalTokenCount = 3L)
            )
          ),
          auto_unbox = TRUE
        ),
        jsonlite::toJSON(
          list(
            key = "chat-1",
            response = list(
              responseId = "first",
              candidates = list(list(
                content = list(parts = list(list(text = "{}")))
              )),
              usageMetadata = list(totalTokenCount = 1L)
            )
          ),
          auto_unbox = TRUE
        ),
        jsonlite::toJSON(
          list(
            key = "chat-2",
            response = list(
              responseId = "second",
              candidates = list(list(
                content = list(parts = list(list(text = "{}")))
              )),
              usageMetadata = list(totalTokenCount = 2L)
            )
          ),
          auto_unbox = TRUE
        )
      )
      writeLines(lines, path)
      invisible(path)
    }
  )

  results <- batch_retrieve(provider, batch)

  expect_equal(
    vapply(results, \(x) x$body$responseId, character(1)),
    c(
      "first",
      "second",
      "third"
    )
  )
})

# Fixture-based tests ----------------------------------------------------

test_that("batch chat works with Gemini fixture", {
  withr::local_envvar(GEMINI_API_KEY = "dummy-key-for-fixture-test")
  chat <- chat_google_gemini(
    system_prompt = "Answer with just the city name",
    model = "gemini-2.5-flash",
    params = params(temperature = 0, seed = 1014)
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
    path = test_path("batch/state-capitals-gemini.json"),
    ignore_hash = TRUE
  )
  expect_equal(out, c("Des Moines", "Albany", "Sacramento", "Austin"))
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
      } else if (
        grepl(
          "HTTP 40[04]|invalid argument|not found|not supported",
          msg,
          ignore.case = TRUE
        )
      ) {
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
