test_that("useful errors", {
  expect_snapshot(error = TRUE, {
    chat()
    chat("")
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

  # We split at the first slash, the remainder is passed to `model`
  chat3 <- chat("openai/provider/model")
  expect_equal(chat3$get_provider()@name, "OpenAI")
  expect_equal(chat3$get_provider()@model, "provider/model")
})

test_that("works for chat functions that don't include `params`", {
  local_mocked_bindings(
    has_ollama = function(...) TRUE,
    models_ollama = function(...) {
      list(id = "qwen3:4b")
    }
  )
  expect_s3_class(chat("ollama/qwen3:4b"), "Chat")

  chat <- chat("ollama/qwen3:4b")
  expect_equal(chat$get_provider()@name, "Ollama")
  expect_equal(chat$get_provider()@model, "qwen3:4b")
})
