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
