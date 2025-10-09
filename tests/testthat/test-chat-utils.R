test_that("find_chat() takes string or chat", {
  chat <- chat_openai_test()
  expect_equal(find_chat(chat), chat)

  expect_equal(find_chat("openai/gpt-4.1-nano"), chat("openai/gpt-4.1-nano"))

  chat <- 1
  expect_snapshot(find_chat(chat), error = TRUE)
})
