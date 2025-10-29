test_that("useful message if no tokens", {
  local_tokens()

  expect_snapshot(token_usage())
})

test_that("can retrieve and log tokens", {
  local_tokens()
  provider <- test_provider("testprovider", "test")

  tokens_log(provider, tokens(input = 1), dollars(0))
  expect_equal(the$tokens, tokens_row("testprovider", "test", 1, 0, 0, 0))

  tokens_log(provider, tokens(output = 1), dollars(0))
  expect_equal(the$tokens, tokens_row("testprovider", "test", 1, 1, 0, 0))

  tokens_log(provider, tokens(cached_input = 1), dollars(0))
  expect_equal(the$tokens, tokens_row("testprovider", "test", 1, 1, 1, 0))

  tokens_log(provider, tokens(), dollars(0))
  expect_equal(the$tokens, tokens_row("testprovider", "test", 1, 1, 1, 0))

  expect_snapshot(token_usage())
})

test_that("can compute price of tokens", {
  provider <- test_provider("OpenAI", "gpt-4o")

  expect_equal(get_token_cost(provider, tokens(input = 1e6)), dollars(2.5))
  expect_equal(get_token_cost(provider, tokens(output = 1e6)), dollars(10))
  expect_equal(
    get_token_cost(provider, tokens(cached_input = 1e6)),
    dollars(1.25)
  )

  # including variant
  expect_equal(
    get_token_cost(provider, tokens(input = 1e6), variant = "priority"),
    dollars(4.25)
  )
  # falling back to base line if no match
  expect_equal(
    get_token_cost(
      provider,
      tokens(input = 1e6),
      variant = "tuesday-afternoon"
    ),
    dollars(2.50)
  )
})

test_that("token_usage() shows price if available", {
  local_tokens()
  provider <- test_provider("OpenAI", "gpt-4o")

  toks <- tokens(input = 1.5e6, output = 2e5, cached_input = 0)
  cost <- get_token_cost(provider, toks)
  tokens_log(provider, toks, cost)
  expect_snapshot(token_usage())
})

test_that("price is formatted nicely", {
  expect_equal(format(dollars(NA)), "NA")
  expect_equal(format(dollars(0.0001)), "$0.00")
  expect_equal(format(dollars(c(10, 1))), c("$10.00", "$ 1.00"))
})
