test_that("ContentJson converted to ContentText", {
  test_provider <- ProviderOpenAI("test", "model", "base_url")
  expect_equal(
    as_json(test_provider, ContentJson(list(x = 1))),
    list(type = "text", text = "{\"x\":1}")
  )
})
