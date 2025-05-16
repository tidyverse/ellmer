test_that("can get chats/data from completed request", {
  chat <- chat_openai_test()

  prompts <- list(
    "What's the capital of Iowa?",
    "What's the capital of New York?",
    "What's the capital of California?",
    "What's the capital of Texas?"
  )
  chats <- batch_chat(
    chat,
    prompts,
    path = test_path("batch/state-capitals.json")
  )
  expect_length(chats, 4)

  type_state <- type_object(name = type_string("State name"))
  data <- batch_chat_structured(
    chat,
    prompts,
    path = test_path("batch/state-name.json"),
    type = type_state
  )
  expect_equal(nrow(data), 4)
})

test_that("errors if chat/provider/prompts don't match previous run", {
  chat <- chat_anthropic_test(system_prompt = "Be cool")
  prompts <- list("What's the capital of Iowa?")
  path <- test_path("batch/state-capitals.json")
  expect_snapshot(batch_chat(chat, prompts, path), error = TRUE)
})

test_that("steps through in logical order, writing to disk at end step", {
  chat <- chat_openai_test()
  prompts <- list("What's your name")
  local_mocked_bindings(
    batch_submit = function(...) list(id = "123"),
    batch_poll = function(...) list(id = "123", results = TRUE),
    batch_status = function(...) list(working = FALSE),
    batch_retrieve = function(...) list(x = 1, y = 2)
  )

  path <- withr::local_tempfile()
  read_stage <- function() jsonlite::read_json(path)$stage

  job <- BatchJob$new(chat, prompts, path)
  completed <- \() batch_chat_completed(chat, prompts, path)

  expect_equal(job$stage, "submitting")
  expect_false(completed())

  job$step()
  expect_equal(job$stage, "waiting")
  expect_equal(read_stage(), "waiting")
  expect_equal(job$batch, list(id = "123"))
  expect_true(completed())

  job$step()
  expect_equal(job$stage, "retrieving")
  expect_equal(read_stage(), "retrieving")
  expect_equal(job$batch, list(id = "123", results = TRUE))
  expect_true(completed())

  job$step()
  expect_equal(job$stage, "done")
  expect_equal(read_stage(), "done")
  expect_equal(job$results, list(x = 1, y = 2))
  expect_true(completed())
})

test_that("can run all steps at once", {
  local_mocked_bindings(
    batch_submit = function(...) list(id = "123"),
    batch_poll = function(...) list(id = "123", results = TRUE),
    batch_status = function(...) list(working = FALSE),
    batch_retrieve = function(...) list(x = 1, y = 2)
  )

  path <- withr::local_tempfile()
  job <- BatchJob$new(
    chat = chat_openai_test(),
    prompts = list("What's your name"),
    path = path
  )
  job$step_until_done()
  expect_equal(job$stage, "done")
  expect_equal(job$results, list(x = 1, y = 2))
})

test_that("errors if wait = FALSE and not complete", {
  local_mocked_bindings(
    batch_submit = function(...) list(id = "123"),
    batch_poll = function(...) list(id = "123", results = TRUE),
    batch_status = function(...) list(working = TRUE)
  )

  path <- withr::local_tempfile()
  job <- BatchJob$new(
    chat = chat_openai_test(),
    prompts = list("What's your name"),
    path = path,
    wait = FALSE
  )
  expect_equal(job$step_until_done(), NULL)
})

test_that("informative error for bad inputs", {
  chat_openai <- chat_openai_test()
  chat_ollama <- chat_openai_test()
  chat_ollama$.__enclos_env__$private$provider <- ProviderOllama(
    "ollama",
    "model",
    "base_url",
    api_key = "api_key"
  )

  expect_snapshot(error = TRUE, {
    batch_chat("x")
    batch_chat(chat_ollama)
    batch_chat(chat_openai, "a")
    batch_chat(chat_openai, list("a"), path = 1)
    batch_chat(chat_openai, list("a"), path = "x", wait = 1)
  })
})
