# prices() --------------------------------------------------------------------

test_that("prices() prefers cached rows over bundled rows on key conflict", {
  local_prices()
  cache_path <- local_prices_cache()

  first_row <- prices_data[1, ]
  first_row$input <- 9999
  attr(first_row, "schema_version") <- attr(prices_data, "schema_version")

  saveRDS(first_row, cache_path)
  result <- prices()

  result_row <- result[
    result$provider == first_row$provider &
      result$model == first_row$model &
      result$variant == first_row$variant,
  ]

  expect_equal(result_row$input, 9999)
})

test_that("prices() informs and falls back when cache schema version is older", {
  local_prices()
  cache_path <- local_prices_cache()

  stale <- prices_data
  attr(stale, "schema_version") <- attr(prices_data, "schema_version") - 1L
  saveRDS(stale, cache_path)

  expect_snapshot(prices())
  expect_equal(the$prices, prices_data)
})

test_that("prices() warns and falls back when cache schema version is newer", {
  local_prices()
  cache_path <- local_prices_cache()

  newer <- prices_data
  attr(newer, "schema_version") <- attr(prices_data, "schema_version") + 1L
  saveRDS(newer, cache_path)

  expect_snapshot(prices())
  expect_equal(the$prices, prices_data)
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

test_that("model_update_prices() aborts with version mismatch advice", {
  local_prices_cache()
  local_mocked_bindings(
    prices_cache_download = function() {
      list(status = "version_mismatch", min_ellmer_version = "0.5.0")
    }
  )

  expect_snapshot(model_update_prices(), error = TRUE)
})

test_that("model_update_prices() aborts when download fails", {
  local_prices_cache()
  local_mocked_bindings(prices_cache_download = function() NA)

  expect_snapshot(model_update_prices(), error = TRUE)
})
