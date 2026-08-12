# Getting started --------------------------------------------------------

test_that("can make simple request", {
  chat <- chat_vllm_test("Be as terse as possible; no punctuation")
  resp <- chat$chat("What is 1 + 1?", echo = FALSE)
  expect_match(resp, "2")
  expect_equal(unname(chat$last_turn()@tokens[1:2] > 0), c(TRUE, TRUE))
})

test_that("can make simple streaming request", {
  chat <- chat_vllm_test("Be as terse as possible; no punctuation")
  resp <- coro::collect(chat$stream("What is 1 + 1?"))
  expect_match(paste0(unlist(resp), collapse = ""), "2")
})

test_that("can list models", {
  test_models(
    \(...) models_vllm("https://llm.nrp-nautilus.io/"),
    has_context_window = TRUE
  )
})

test_that("model listing reports the context window", {
  local_mocked_responses(function(req) {
    response_json(
      body = list(
        data = list(
          list(id = "qwen3", max_model_len = 40960),
          list(id = "unknown")
        )
      )
    )
  })

  models <- models_vllm("https://example.com", credentials = \() "")
  expect_equal(models$context_window, c(40960L, NA_integer_))
})
