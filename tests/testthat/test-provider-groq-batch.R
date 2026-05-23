# Batch support -----------------------------------------------------------

test_that("ProviderGroq has batch support", {
  chat <- chat_groq(credentials = function() list(Authorization = "Bearer x"))
  expect_true(has_batch_support(chat$get_provider()))
})

test_that("batch_status parses completed state", {
  provider <- chat_groq(
    credentials = function() list(Authorization = "Bearer x")
  )$get_provider()
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
  provider <- chat_groq(
    credentials = function() list(Authorization = "Bearer x")
  )$get_provider()
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
  provider <- chat_groq(
    credentials = function() list(Authorization = "Bearer x")
  )$get_provider()
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

test_that("batch_chat_structured works with Groq fixture", {
  withr::local_envvar(GROQ_API_KEY = "dummy-key-for-fixture-test")
  chat <- chat_groq(
    system_prompt = "Answer with just the city name",
    model = "openai/gpt-oss-20b",
    params = params(temperature = 0, seed = 1014)
  )

  prompts <- list(
    "What's the capital of Iowa?",
    "What's the capital of New York?",
    "What's the capital of California?",
    "What's the capital of Texas?"
  )

  out <- batch_chat_structured(
    chat,
    prompts,
    path = test_path("batch/state-capitals-groq-structured.json"),
    type = type_object(capital = type_string()),
    ignore_hash = TRUE
  )

  expect_s3_class(out, "data.frame")
  expect_equal(out$capital, c("Des Moines", "Albany", "Sacramento", "Austin"))
})

# Integration tests -------------------------------------------------------

test_that("Groq batch_chat submits and can be resumed", {
  skip_if(Sys.getenv("GROQ_API_KEY") == "", "No Groq credentials set")

  chat <- chat_groq(
    system_prompt = "Reply concisely",
    model = "llama-3.1-8b-instant"
  )

  prompts <- list("Reply with exactly: ok")
  results_file <- withr::local_tempfile(fileext = ".json")

  chats <- tryCatch(
    batch_chat(chat, prompts = prompts, path = results_file, wait = FALSE),
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
    for (i in seq_len(12)) {
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
  expect_s3_class(chats[[1]], "Chat")
})
