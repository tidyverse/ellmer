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

test_that("deprecated Provider properties warn but still work", {
  chat <- Chat$new(provider = test_provider(), model = test_model("m"))
  provider <- chat$get_provider()
  expect_snapshot({
    provider@model
    provider@params
    provider@extra_args
    provider <- Provider(
      name = "test",
      base_url = "https://example.com",
      model = "x"
    )
    provider@model
  })
})

test_that("Provider print omits deprecated properties", {
  expect_snapshot(print(test_provider()))
})

test_that("model_max_context_length() is NA when the provider can't list models", {
  local_model_listings()
  provider <- Provider(
    name = "test",
    base_url = "https://example.com"
  )
  expect_equal(model_max_context_length(provider, "model"), NA_integer_)
})

test_that("model_max_context_length() is NA when the listing reports none", {
  local_model_listings()
  local_mocked_responses(function(req) {
    response_json(body = list(data = list(list(id = "qwen3"))))
  })
  provider <- ProviderVllm(
    name = "VLLM",
    base_url = "https://example.com",
    credentials = \() ""
  )
  expect_equal(model_max_context_length(provider, "qwen3"), NA_integer_)
  expect_equal(model_max_context_length(provider, "not-a-model"), NA_integer_)
})

test_that("model_max_context_length() lists the provider's models once", {
  local_model_listings()
  requests <- 0
  local_mocked_responses(function(req) {
    requests <<- requests + 1
    response_json(
      body = list(
        data = list(
          list(id = "qwen3", max_model_len = 40960),
          list(id = "llama3", max_model_len = 8192)
        )
      )
    )
  })
  provider <- ProviderVllm(
    name = "VLLM",
    base_url = "https://example.com",
    credentials = \() ""
  )

  expect_equal(model_max_context_length(provider, "qwen3"), 40960L)
  expect_equal(model_max_context_length(provider, "llama3"), 8192L)
  expect_equal(requests, 1)
})

test_that("model_max_context_length() is NA if listing fails, and tries again", {
  local_model_listings()
  provider <- ProviderVllm(
    name = "VLLM",
    base_url = "https://example.com",
    credentials = \() ""
  )

  local_mocked_responses(function(req) {
    response_json(status_code = 404L, body = list(error = "Not Found"))
  })
  expect_equal(model_max_context_length(provider, "qwen3"), NA_integer_)

  local_mocked_responses(function(req) {
    response_json(
      body = list(data = list(list(id = "qwen3", max_model_len = 40960)))
    )
  })
  expect_equal(model_max_context_length(provider, "qwen3"), 40960L)
})

test_that("match_model_id() matches undated aliases and Ollama's default tag", {
  ids <- c("claude-opus-4-1-20250805", "claude-opus-4-20250514", "llama3.2")
  expect_equal(match_model_id("claude-opus-4", ids), 2L)
  expect_equal(match_model_id("claude-opus-4-1", ids), 1L)
  expect_equal(match_model_id("claude-opus", ids), NA_integer_)
  expect_equal(match_model_id("llama3.2:latest", ids), 3L)

  snapshots <- c("claude-3-5-sonnet-20240620", "claude-3-5-sonnet-20241022")
  expect_equal(match_model_id("claude-3-5-sonnet", snapshots), 2L)
})
