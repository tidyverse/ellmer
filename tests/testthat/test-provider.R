test_that("ContentJson converted to ContentText", {
  test_provider <- ProviderOpenAICompatible("test", "model", "base_url")
  expect_equal(
    as_json(test_provider, ContentJson(list(x = 1))),
    list(type = "text", text = "{\"x\":1}")
  )
})

test_that("models_list() on base Provider throws not_implemented error", {
  provider <- Provider(
    name = "test",
    base_url = "https://example.com"
  )
  expect_error(models_list(provider), class = "not_implemented")
})

test_that("models_list() dispatches through Chat to provider", {
  provider <- Provider(
    name = "test",
    base_url = "https://example.com"
  )
  chat <- Chat$new(provider = provider, model = test_model())
  expect_error(models_list(chat), class = "not_implemented")
})

test_that("model_context_window() is NA when the provider can't list models", {
  local_context_windows()
  provider <- Provider(
    name = "test",
    base_url = "https://example.com"
  )
  expect_equal(model_context_window(provider, "model"), NA_integer_)
})

test_that("model_context_window() is NA when the listing reports no window", {
  local_context_windows()
  local_mocked_responses(function(req) {
    response_json(body = list(data = list(list(id = "qwen3"))))
  })
  provider <- ProviderVllm(
    name = "VLLM",
    base_url = "https://example.com",
    credentials = \() ""
  )
  expect_equal(model_context_window(provider, "qwen3"), NA_integer_)
  expect_equal(model_context_window(provider, "not-a-model"), NA_integer_)
})

test_that("model_context_window() only requests the model listing once", {
  local_context_windows()
  requests <- 0
  local_mocked_responses(function(req) {
    requests <<- requests + 1
    response_json(
      body = list(data = list(list(id = "qwen3", max_model_len = 40960)))
    )
  })
  provider <- ProviderVllm(
    name = "VLLM",
    base_url = "https://example.com",
    credentials = \() ""
  )

  expect_equal(model_context_window(provider, "qwen3"), 40960L)
  expect_equal(model_context_window(provider, "qwen3"), 40960L)
  expect_equal(requests, 1)
})
