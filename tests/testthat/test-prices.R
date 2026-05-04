local_prices <- function(frame = parent.frame()) {
  old <- the$prices
  the$prices <- NULL
  defer(the$prices <- old, envir = frame)
}

# prices_cache_stale() --------------------------------------------------------

test_that("prices_cache_stale() returns TRUE when file is missing", {
  local_prices_cache()
  expect_true(prices_cache_stale())
})

test_that("prices_cache_stale() returns FALSE for a recently written file", {
  cache_path <- local_prices_cache()
  saveRDS(data.frame(), cache_path)
  expect_false(prices_cache_stale())
})

test_that("prices_cache_stale() returns TRUE for a file older than 7 days", {
  cache_path <- local_prices_cache()
  saveRDS(data.frame(), cache_path)
  old_time <- Sys.time() - as.difftime(8, units = "days")
  Sys.setFileTime(cache_path, old_time)
  expect_true(prices_cache_stale())
})

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
    result$provider == bundled[1, "provider"] &
      result$model == bundled[1, "model"] &
      result$variant == bundled[1, "variant"],
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
    result$provider == bundled[1, "provider"] &
      result$model == bundled[1, "model"] &
      result$variant == bundled[1, "variant"],
  ]
  expect_equal(result_row$input, 9999)
})

test_that("prices() uses bundled prices when no cache exists", {
  local_prices()
  local_prices_cache()

  prices()
  expect_equal(the$prices, prices_data)
})

# prices_update() -------------------------------------------------------------

test_that("prices_update() does nothing during testing (CRAN-safe)", {
  cache_path <- local_prices_cache()

  prices_update()

  expect_false(file.exists(cache_path))
})

test_that("prices_update() does nothing when opt-out option is set", {
  local_prices_cache()
  withr::local_options(ellmer.update_prices = FALSE)

  prices_update()

  expect_false(file.exists(prices_cache_path()))
})

test_that("prices_update() does nothing when opt-out env var is set", {
  local_prices_cache()
  withr::local_envvar(ELLMER_UPDATE_PRICES = "false")

  prices_update()

  expect_false(file.exists(prices_cache_path()))
})

test_that("prices_update() does nothing when cache is fresh", {
  cache_path <- local_prices_cache()
  local_prices()

  dummy <- data.frame(
    provider = "Test",
    model = "test-model",
    variant = "",
    input = 1,
    output = 2,
    cached_input = 0.5
  )
  saveRDS(dummy, cache_path)

  old_mtime <- file.mtime(cache_path)
  prices_update()

  expect_equal(file.mtime(cache_path), old_mtime)
})
