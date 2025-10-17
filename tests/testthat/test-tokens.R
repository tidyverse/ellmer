test_that("useful message if no tokens", {
  local_tokens()

  expect_snapshot(token_usage())
})

test_that("can retrieve and log tokens", {
  local_tokens()
  provider <- test_provider("testprovider", "test")

  tokens_log(provider, tokens(input = 1))
  expect_equal(the$tokens, tokens_row("testprovider", "test", "", 1, 0, 0))

  tokens_log(provider, tokens(output = 1))
  expect_equal(the$tokens, tokens_row("testprovider", "test", "", 1, 1, 0))

  tokens_log(provider, tokens(cached_input = 1))
  expect_equal(the$tokens, tokens_row("testprovider", "test", "", 1, 1, 1))

  tokens_log(provider, tokens())
  expect_equal(the$tokens, tokens_row("testprovider", "test", "", 1, 1, 1))

  expect_snapshot(token_usage())
})

test_that("can compute price of tokens", {
  expect_equal(get_token_cost("OpenAI", "gpt-4o", "", 1e6, 0, 0), dollars(2.5))
  expect_equal(get_token_cost("OpenAI", "gpt-4o", "", 0, 1e6, 0), dollars(10))
  expect_equal(get_token_cost("OpenAI", "gpt-4o", "", 0, 0, 1e6), dollars(1.25))

  # including variant
  expect_equal(
    get_token_cost("OpenAI", "gpt-4o", "priority", 1e6),
    dollars(4.25)
  )
  # falling back to base line if no match
  expect_equal(
    get_token_cost("OpenAI", "gpt-4o", "tuesday-afternoon", 1e6),
    dollars(2.50)
  )
})

test_that("token_usage() shows price if available", {
  local_tokens()
  provider <- test_provider("OpenAI", "gpt-4o")

  tokens_log(provider, tokens(input = 1.5e6, output = 2e5, cached_input = 0))
  expect_snapshot(token_usage())
})

test_that("price is formatted nicely", {
  expect_equal(format(dollars(NA)), "NA")
  expect_equal(format(dollars(0.0001)), "$0.00")
  expect_equal(format(dollars(c(10, 1))), c("$10.00", "$ 1.00"))
})
