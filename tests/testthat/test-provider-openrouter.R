# Getting started --------------------------------------------------------

test_that("can make simple request", {
  chat <- chat_openrouter("Be as terse as possible; no punctuation", model = "gpt-4o-mini")
  resp <- chat$chat("What is 1 + 1?", echo = FALSE)
  expect_match(resp, "2")
  expect_true(all(chat$last_turn()@tokens >= 1))
})

test_that("can make simple streaming request", {
  chat <- chat_openrouter("Be as terse as possible; no punctuation", model = "gpt-4o-mini")
  resp <- coro::collect(chat$stream("What is 1 + 1?"))
  expect_match(paste0(unlist(resp), collapse = ""), "2")
})

test_that("handles errors", {
  chat <- chat_openrouter(api_args = list(temperature = "hot"), model = "gpt-4o-mini")
  expect_snapshot(error = TRUE, {
    chat$chat("What is 1 + 1?", echo = FALSE)
    chat$chat("What is 1 + 1?", echo = TRUE)
  })
})

# Common provider interface -----------------------------------------------

test_that("respects turns interface", {
  chat_fun <- function(...) chat_openrouter(..., model = "gpt-4o-mini")

  test_turns_system(chat_fun)
  test_turns_existing(chat_fun)
})

test_that("all tool variations work", {
  chat_fun <- function(...) chat_openrouter(..., model = "gpt-4o-mini")

  test_tools_simple(chat_fun)
  test_tools_async(chat_fun)
  test_tools_parallel(chat_fun)
  test_tools_sequential(chat_fun, total_calls = 6)
})

test_that("can extract data", {
  chat_fun <- function(...) chat_openrouter(..., model = "gpt-4o-mini")

  test_data_extraction(chat_fun)
})

test_that("can use images", {
  chat_fun <- function(...) chat_openrouter(..., model = "gpt-4o-mini")

  test_images_inline(chat_fun)
  test_images_remote(chat_fun)
})
