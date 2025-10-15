test_that("useful message if no tokens", {
  local_tokens()

  expect_snapshot(token_usage())
})

test_that("can retrieve and log tokens", {
  local_tokens()
  provider <- test_provider("testprovider", "test")

  tokens_log(provider, c(input = 0, output = 0, cached_input = 100))
  expect_equal(
    the$tokens,
    tokens_row(
      "testprovider",
      "test",
      c(input = 0, output = 0, cached_input = 100)
    )
  )

  tokens_log(provider, c(input = 10, output = 50, cached_input = 0))
  expect_equal(
    the$tokens,
    tokens_row(
      "testprovider",
      "test",
      c(input = 10, output = 50, cached_input = 100)
    )
  )

  tokens_log(provider, c(input = 0, output = 10, cached_input = 0))
  expect_equal(
    the$tokens,
    tokens_row(
      "testprovider",
      "test",
      c(input = 10, output = 60, cached_input = 100)
    )
  )

  tokens_log(provider, c(input = 0, output = 0, cached_input = 0))
  expect_equal(
    the$tokens,
    tokens_row(
      "testprovider",
      "test",
      c(input = 10, output = 60, cached_input = 100)
    )
  )

  expect_snapshot(token_usage())
})

test_that("can compute price of tokens", {
  expect_equal(get_token_cost("OpenAI", "gpt-4o", 1e6, 0, 0), dollars(2.5))
  expect_equal(get_token_cost("OpenAI", "gpt-4o", 0, 1e6, 0), dollars(10))
  expect_equal(get_token_cost("OpenAI", "gpt-4o", 0, 0, 1e6), dollars(1.25))
})

test_that("token_usage() shows price if available", {
  local_tokens()
  local_mocked_bindings(
    prices = data.frame(
      provider = "testprovider",
      model = "test",
      input = 0.10,
      output = 0.01,
      cached_input = 0
    )
  )
  provider <- test_provider("testprovider", "test")

  tokens_log(provider, c(input = 123e5, output = 678e3, cached_input = 0))
  expect_snapshot(token_usage())
})

test_that("price is formatted nicely", {
  expect_equal(format(dollars(NA)), "NA")
  expect_equal(format(dollars(0.0001)), "$0.00")
  expect_equal(format(dollars(c(10, 1))), c("$10.00", "$ 1.00"))
})
