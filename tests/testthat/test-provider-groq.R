# Common provider interface -----------------------------------------------

test_that("defaults are reported", {
  expect_snapshot(. <- chat_groq())
})

test_that("supports tool calling", {
  chat_fun <- chat_groq
  test_tools_simple(chat_fun)
})

test_that("supports structured data", {
  # current default model does not support structured data
  chat_fun <- function(model = "openai/gpt-oss-20b", ...) {
    chat_groq(model = model, ...)
  }
  test_data_extraction(chat_fun)
})
