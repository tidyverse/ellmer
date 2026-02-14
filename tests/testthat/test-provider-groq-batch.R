# Helper to create a dummy provider without needing real credentials
dummy_groq_provider <- function() {
  ProviderGroq(
    name = "Groq",
    base_url = "https://api.groq.com/openai/v1",
    model = "llama-3.1-8b-instant",
    params = params(),
    extra_args = list(),
    extra_headers = character(),
    credentials = NULL
  )
}

# Groq batch helper functions -----------------------------------------------

test_that("groq_json_fallback extracts custom_id from malformed line", {
  result <- groq_json_fallback('{"custom_id": "chat-3", broken json...')

  expect_equal(result$custom_id, "chat-3")
  expect_equal(result$response$status_code, 500)
})

test_that("groq_json_fallback returns NA for unparseable line", {
  result <- groq_json_fallback("completely broken")

  expect_true(is.na(result$custom_id))
  expect_equal(result$response$status_code, 500)
})

# Turn serialization -------------------------------------------------------

test_that("as_json(Turn) handles ContentJson in assistant turns", {
  provider <- dummy_groq_provider()

  # ContentJson with data
  turn_json <- AssistantTurn(list(ContentJson(data = list(answer = "4"))))
  result <- as_json(provider, turn_json)
  expect_equal(result[[1]]$role, "assistant")
  expect_equal(result[[1]]$content, '{"answer":"4"}')

  # ContentJson with string
  turn_str <- AssistantTurn(list(ContentJson(data = NULL, string = '{"x":1}')))
  result2 <- as_json(provider, turn_str)
  expect_equal(result2[[1]]$content, '{"x":1}')

  # ContentText still works
  turn_text <- AssistantTurn(list(ContentText("hello")))
  result3 <- as_json(provider, turn_text)
  expect_equal(result3[[1]]$content, "hello")
})

# Schema generation --------------------------------------------------------

test_that("as_json(TypeObject) adds additionalProperties: false", {
  provider <- dummy_groq_provider()

  type_obj <- type_object(
    name = type_string(),
    age = type_integer()
  )
  schema <- as_json(provider, type_obj)

  expect_equal(schema$type, "object")
  expect_equal(schema$additionalProperties, FALSE)
  expect_true("name" %in% names(schema$properties))
  expect_true("age" %in% names(schema$properties))
})

test_that("nested objects get recursive additionalProperties: false", {
  provider <- dummy_groq_provider()

  type_nested <- type_object(
    person = type_object(
      name = type_string(),
      age = type_integer()
    ),
    address = type_object(
      city = type_string(),
      country = type_string()
    )
  )
  schema <- as_json(provider, type_nested)

  expect_equal(schema$additionalProperties, FALSE)
  expect_equal(schema$properties$person$additionalProperties, FALSE)
  expect_equal(schema$properties$address$additionalProperties, FALSE)
})

test_that("array schema items get additionalProperties: false", {
  provider <- dummy_groq_provider()

  type_arr <- type_array(
    type_object(name = type_string())
  )
  schema <- as_json(provider, type_arr)

  expect_equal(schema$type, "array")
  expect_equal(schema$items$type, "object")
  expect_equal(schema$items$additionalProperties, FALSE)
})

# Batch support -----------------------------------------------------------

test_that("ProviderGroq has batch support", {
  provider <- dummy_groq_provider()
  expect_true(has_batch_support(provider))
})

test_that("batch_status parses completed state", {
  provider <- dummy_groq_provider()
  batch <- list(
    status = "completed",
    request_counts = list(total = 5L, completed = 5L, failed = 0L)
  )
  status <- batch_status(provider, batch)

  expect_false(status$working)
  expect_equal(status$n_processing, 0L)
  expect_equal(status$n_succeeded, 5L)
  expect_equal(status$n_failed, 0L)
})

test_that("batch_status parses in_progress state", {
  provider <- dummy_groq_provider()
  batch <- list(
    status = "in_progress",
    request_counts = list(total = 10L, completed = 3L, failed = 1L)
  )
  status <- batch_status(provider, batch)

  expect_true(status$working)
  expect_equal(status$n_processing, 6L)
  expect_equal(status$n_succeeded, 3L)
  expect_equal(status$n_failed, 1L)
})

test_that("batch_status clamps n_processing to zero", {
  provider <- dummy_groq_provider()
  batch <- list(
    status = "completed",
    request_counts = list(total = 5L, completed = 6L, failed = 0L)
  )
  status <- batch_status(provider, batch)

  expect_equal(status$n_processing, 0L)
})

# Fixture-based tests ----------------------------------------------------

test_that("batch chat works with Groq fixture", {
  withr::local_envvar(GROQ_API_KEY = "dummy-key-for-fixture-test")
  chat <- chat_groq(
    system_prompt = "Answer with just the city name",
    model = "llama-3.1-8b-instant",
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
    path = test_path("batch/state-capitals-groq.json"),
    ignore_hash = TRUE
  )
  expect_equal(out, c("Des Moines", "Albany", "Sacramento", "Austin"))
})

# Integration tests -------------------------------------------------------

test_that("Groq batch_chat submits and can be resumed", {
  skip_if(
    Sys.getenv("GROQ_API_KEY") == "",
    "No Groq credentials set"
  )

  chat <- chat_groq(
    system_prompt = "Reply concisely",
    model = "llama-3.1-8b-instant"
  )

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
    for (i in seq_len(60)) {
      Sys.sleep(10)
      completed <- isTRUE(batch_chat_completed(chat, prompts, results_file))
      if (completed) break
    }

    if (!completed) {
      skip("Groq batch did not complete within test timeout.")
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

test_that("Groq batch_chat_structured works", {
  skip_if(
    Sys.getenv("GROQ_API_KEY") == "",
    "No Groq credentials set"
  )

  chat <- chat_groq(
    system_prompt = "Reply concisely",
    model = "llama-3.1-8b-instant"
  )

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
      } else {
        stop(e)
      }
    }
  )

  if (is.null(result)) {
    completed <- FALSE
    for (i in seq_len(60)) {
      Sys.sleep(10)
      completed <- isTRUE(batch_chat_completed(chat, prompts, results_file))
      if (completed) break
    }

    if (!completed) {
      skip("Groq batch did not complete within test timeout.")
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
