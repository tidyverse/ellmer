test_that("useful errors", {
  expect_snapshot(error = TRUE, {
    chat()
    chat("a/b/c")
    chat("susan")
    chat("susan/jones")
  })
})


test_that("can set model or use default", {
  chat1 <- chat("openai")
  expect_equal(chat1$get_provider()@name, "OpenAI")
  expect_equal(chat1$get_provider()@model, chat_openai()$get_provider()@model)

  chat2 <- chat("openai/gpt-4.1-mini")
  expect_equal(chat2$get_provider()@name, "OpenAI")
  expect_equal(chat2$get_provider()@model, "gpt-4.1-mini")
})
