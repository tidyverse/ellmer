test_that("useful message if no tokens", {
  local_tokens()
  
  expect_snapshot(token_usage())
})

test_that("can retrieve and log tokens", {
  local_tokens()

  provider <- test_provider("testprovider", "test")
  tokens_log(provider, c(10, 50))
  tokens_log(provider, c(0, 10))

  df <- token_usage()
  expect_equal(df$input[df$name == "testprovider/test"], 10)
  expect_equal(df$output[df$name == "testprovider/test"], 60)

  expect_snapshot(token_usage())
})
