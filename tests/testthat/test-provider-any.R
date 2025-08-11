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

test_that("works for chat functions that don't include `params`", {
  model <- chat_ollama_test("qwen3:4b")$get_provider()@model
  chat <- chat(paste0("ollama/", model))
  expect_equal(chat$get_provider()@name, "Ollama")
  expect_equal(chat$get_provider()@model, "qwen3:4b")
})

test_that("warns if given arguments that aren't used by provider", {
  expect_snapshot(
    chat("openai/gpt-4.1-mini", foo = "bar")
  )

  skip_if(
    "params" %in% fn_fmls_names(chat_ollama),
    "Update `test-provider-any.R`, `chat_ollama` now accepts `params`."
  )

  model <- chat_ollama_test("qwen3:4b")$get_provider()@model
  expect_snapshot(
    chat(paste0("ollama/", model), params = params(temperature = 0.5))
  )
})

test_that("requires `model` and `system_prompt` arguments", {
  expect_snapshot(error = TRUE, {
    chat("cortex_analyst")
  })
})
