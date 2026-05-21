test_that("ContentJson converted to ContentText", {
  test_provider <- ProviderOpenAICompatible("test", "model", "base_url")
  expect_equal(
    as_json(test_provider, ContentJson(list(x = 1))),
    list(type = "text", text = "{\"x\":1}")
  )
})

test_that("get_models() on base Provider throws not_implemented error", {
  provider <- Provider(
    name = "test",
    model = "test",
    base_url = "https://example.com"
  )
  expect_error(get_models(provider), class = "not_implemented")
})

test_that("get_models() dispatches through Chat to provider", {
  provider <- Provider(
    name = "test",
    model = "test",
    base_url = "https://example.com"
  )
  chat <- Chat$new(provider = provider)
  expect_error(get_models(chat), class = "not_implemented")
})
