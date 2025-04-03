test_that("useful message if no tokens", {
  local_tokens()

  expect_snapshot(token_usage())
})

test_that("can retrieve and log tokens", {
  local_tokens()
  provider <- test_provider("testprovider", "test")

  expect_equal(tokens_log(provider, 10, 50), c(10, 50))
  expect_equal(the$tokens, tokens_row("testprovider", "test", 10, 50))

  expect_equal(tokens_log(provider, 0, 10), c(0, 10))
  expect_equal(the$tokens, tokens_row("testprovider", "test", 10, 60))

  expect_equal(tokens_log(provider), c(0, 0))
  expect_equal(the$tokens, tokens_row("testprovider", "test", 10, 60))

  expect_snapshot(token_usage())
})

test_that("can compute price of tokens", {
  expect_equal(get_token_cost("OpenAI", "gpt-4o", 1e6, 0), dollars(2.5))
  expect_equal(get_token_cost("OpenAI", "gpt-4o", 0, 1e6), dollars(10))
})

test_that("token_usage() shows price if available", {
  local_tokens()
  local_mocked_bindings(
    prices = data.frame(
      provider = "testprovider",
      model = "test",
      input = 0.10,
      output = 0.01
    )
  )
  provider <- test_provider("testprovider", "test")

  tokens_log(provider, 123e5, 678e3)
  expect_snapshot(token_usage())
})

test_that("price is formatted nicely", {
  expect_equal(format(dollars(NA)), "NA")
  expect_equal(format(dollars(0.0001)), "$0.00")
  expect_equal(format(dollars(c(10, 1))), c("$10.00", "$ 1.00"))
})
