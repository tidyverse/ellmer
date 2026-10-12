# Getting started --------------------------------------------------------

test_that("can make simple request", {
  chat <- chat_lmstudio_test("Be as terse as possible; no punctuation")
  resp <- chat$chat("What is 1 + 1?", echo = FALSE)
  expect_match(resp, "2")
  expect_equal(unname(chat$last_turn()@tokens[1:2] > 0), c(TRUE, TRUE))
})

test_that("can make simple streaming request", {
  chat <- chat_lmstudio_test("Be as terse as possible; no punctuation")
  resp <- coro::collect(chat$stream("What is 1 + 1?"))
  expect_match(paste0(unlist(resp), collapse = ""), "2")
})

test_that("can list models", {
  skip_if_no_lmstudio()
  test_models(models_lmstudio)
})

test_that("includes list of models in error message if `model` is missing", {
  skip_if_no_lmstudio()

  local_mocked_bindings(
    models_lmstudio = function(...) {
      list(id = c("llama3", "google/gemma-4-26b-a4b"))
    }
  )

  expect_snapshot(chat_lmstudio(), error = TRUE)
})

test_that("checks that requested model is loaded", {
  skip_if_no_lmstudio()
  local_mocked_bindings(
    models_lmstudio = function(...) list(id = "llama3")
  )
  expect_snapshot(
    chat_lmstudio(model = "not-a-real-model"),
    error = TRUE
  )
})

# Common provider interface -----------------------------------------------

test_that("supports tool calling", {
  chat_fun <- chat_lmstudio_test
  test_tools_simple(chat_fun)
})
