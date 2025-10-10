test_that("as_chat() takes string or chat", {
  chat <- chat_openai_test()
  expect_equal(as_chat(chat), chat)

  expect_equal(as_chat("openai/gpt-4.1-nano"), chat("openai/gpt-4.1-nano"))

  chat <- 1
  expect_snapshot(as_chat(chat), error = TRUE)
})
