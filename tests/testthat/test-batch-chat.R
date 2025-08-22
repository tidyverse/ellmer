test_that("can get chats/data from completed request", {
  # Ensure we use the canonical OpenAI base URL to match stored batch state
  withr::local_envvar(OPENAI_BASE_URL = "")
  chat <- chat_openai_test()

  prompts <- list(
    "What's the capital of Iowa?",
    "What's the capital of New York?",
    "What's the capital of California?",
    "What's the capital of Texas?"
  )

  # Use a temp copy of the stored state, and update the provider hash so
  # the test is portable across environments (e.g., different API keys)
  src <- test_path("batch/state-capitals.json")
  tmp <- withr::local_tempfile(fileext = ".json")
  file.copy(src, tmp, overwrite = TRUE)

  state <- jsonlite::read_json(tmp, simplifyVector = FALSE)
  prov <- ellmer:::provider_hash(chat$get_provider())
  state$hash$provider <- ellmer:::hash(prov)
  # Recompute prompt + existing turn hashes to reflect this environment
  user_turns <- ellmer:::as_user_turns(prompts)
  state$hash$prompts <- ellmer:::hash(lapply(user_turns, format))
  state$hash$user_turns <- ellmer:::hash(lapply(chat$get_turns(TRUE), format))
  jsonlite::write_json(state, tmp, auto_unbox = TRUE, pretty = TRUE)

  chats <- batch_chat(chat, prompts, path = tmp)
  expect_length(chats, 4)

  out <- batch_chat_text(chat, prompts, path = tmp)
  expect_equal(out, c("Des Moines", "Albany", "Sacramento", "Austin"))

  # Do the same for the structured data test
  src2 <- test_path("batch/state-name.json")
  tmp2 <- withr::local_tempfile(fileext = ".json")
  file.copy(src2, tmp2, overwrite = TRUE)
  state2 <- jsonlite::read_json(tmp2, simplifyVector = FALSE)
  state2$hash$provider <- ellmer:::hash(prov)
  state2$hash$prompts <- ellmer:::hash(lapply(user_turns, format))
  state2$hash$user_turns <- ellmer:::hash(lapply(chat$get_turns(TRUE), format))
  jsonlite::write_json(state2, tmp2, auto_unbox = TRUE, pretty = TRUE)

  type_state <- type_object(name = type_string("State name"))
  data <- batch_chat_structured(chat, prompts, path = tmp2, type = type_state)
  expect_equal(nrow(data), 4)
})

test_that("errors if chat/provider/prompts don't match previous run", {
  chat <- chat_anthropic_test(system_prompt = "Be cool")
  prompts <- list("What's the capital of Iowa?")
  path <- test_path("batch/state-capitals.json")
  expect_snapshot(batch_chat(chat, prompts, path), error = TRUE)

  expect_snapshot(
    batch_chat_structured(chat, prompts, path, type = type_string()),
    error = TRUE
  )
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
