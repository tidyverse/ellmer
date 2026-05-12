# prices() --------------------------------------------------------------------

test_that("prices() prefers cached rows over bundled rows on key conflict", {
  local_prices()
  cache_path <- local_prices_cache()

  bundled <- prices_data
  first_row <- bundled[1, ]
  first_row$input <- 9999

  saveRDS(first_row, cache_path)
  result <- prices()

  result_row <- result[
    result$provider == first_row$provider &
      result$model == first_row$model &
      result$variant == first_row$variant,
  ]

  expect_equal(result_row$input, 9999)
})

test_that("prices() handles cached data with missing columns", {
  local_prices()
  cache_path <- local_prices_cache()

  bundled <- prices_data
  cached_row <- bundled[1, ]
  cached_row$input <- 9999
  cached_row$cached_input <- NULL

  saveRDS(cached_row, cache_path)
  result <- prices()

  result_row <- result[
    result$provider == cached_row$provider &
      result$model == cached_row$model &
      result$variant == cached_row$variant,
  ]

  expect_equal(result_row$input, 9999)
  expect_true(is.na(result_row$cached_input))
})

test_that("prices() handles cached data with extra columns", {
  local_prices()
  cache_path <- local_prices_cache()

  bundled <- prices_data
  cached_row <- bundled[1, ]
  cached_row$input <- 9999
  cached_row$extra_col <- "surprise"

  saveRDS(cached_row, cache_path)
  result <- prices()

  expect_false("extra_col" %in% names(result))
  result_row <- result[
    result$provider == cached_row$provider &
      result$model == cached_row$model &
      result$variant == cached_row$variant,
  ]
  expect_equal(result_row$input, 9999)
})

test_that("prices() uses bundled prices when no cache exists", {
  local_prices()
  local_prices_cache()

  prices()
  expect_equal(the$prices, prices_data)
})

# model_update_prices() -------------------------------------------------------

test_that("model_update_prices() informs and returns TRUE when download succeeds", {
  local_prices_cache()
  the$prices <- prices_data
  local_mocked_bindings(prices_cache_download = function() TRUE)

  expect_snapshot(result <- model_update_prices())
  expect_true(result)
  expect_null(the$prices)
})

test_that("model_update_prices() informs and returns FALSE when already up to date", {
  local_prices_cache()
  local_mocked_bindings(prices_cache_download = function() FALSE)

  expect_snapshot(result <- model_update_prices())
  expect_false(result)
})

test_that("model_update_prices() aborts when download fails", {
  local_prices_cache()
  local_mocked_bindings(prices_cache_download = function() NA)

  expect_snapshot(model_update_prices(), error = TRUE)
})
