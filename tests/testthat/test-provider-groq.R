# Common provider interface -----------------------------------------------

test_that("defaults are reported", {
  expect_snapshot(. <- chat_groq())
})

test_that("supports tool calling", {
  chat_fun <- function(...) chat_groq(..., model = "Llama-3.3-70b-Versatile")

  test_tools_simple(chat_fun)
})
