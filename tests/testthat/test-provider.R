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
    model = "test",
    base_url = "https://example.com"
  )
  expect_error(models_list(provider), class = "not_implemented")
})

test_that("models_list() dispatches through Chat to provider", {
  provider <- Provider(
    name = "test",
    model = "test",
    base_url = "https://example.com"
  )
  chat <- Chat$new(provider = provider)
  expect_error(models_list(chat), class = "not_implemented")
})
