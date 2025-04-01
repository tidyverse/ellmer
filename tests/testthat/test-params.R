# params -------------------------------------------------------------------

test_that("NULL values are stripped", {
  expect_equal(params(), set_names(list()))
})

test_that("checks its inputs", {
  expect_snapshot(error = TRUE, {
    params(temperature = "x")
    params(top_p = "x")
    params(top_k = "x")
    params(frequency_penalty = "x")
    params(presence_penalty = "x")
    params(seed = "x")
    params(max_tokens = "x")
    params(log_probs = 1)
    params(stop_sequences = 1)
  })
})

# standardise_params -------------------------------------------------------

test_that("standardise_params warns about unknown args", {
  test_params <- list(temperature = 0.7, top_p = 0.9)
  provider_params <- c("temperature" = "temperature")
  expect_snapshot(. <- standardise_params(test_params, provider_params))
})

test_that("standardise_params renames supported parameters", {
  test_params <- list(
    top_p = 0.9,
    temperature = 0.7,
    max_tokens = 100
  )

  provider_params <- c(
    temp = "temperature",
    topP = "top_p",
    maxTokens = "max_tokens"
  )
  expect_equal(
    standardise_params(test_params, provider_params),
    list(topP = 0.9, temp = 0.7, maxTokens = 100)
  )
})

test_that("standardise_params handles empty parameters correctly", {
  test_params <- list(extra_args = list())
  provider_params <- c("temperature" = "temperature", "top_p" = "top_p")
  expect_equal(standardise_params(test_params, provider_params), list())
})

test_that("standardise_params leavees extra_args as is", {
  test_params <- list(top_k = 2, extra_args = list(a = 1, b = 2))

  provider_params <- c(top_k = "top_k")
  expect_equal(
    standardise_params(test_params, provider_params),
    list(top_k = 2, a = 1, b = 2)
  )
})
