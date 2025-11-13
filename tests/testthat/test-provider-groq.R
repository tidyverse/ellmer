# Common provider interface -----------------------------------------------

test_that("defaults are reported", {
  expect_snapshot(. <- chat_groq())
})

test_that("supports tool calling", {
  chat_fun <- chat_groq
  test_tools_simple(chat_fun)
})
