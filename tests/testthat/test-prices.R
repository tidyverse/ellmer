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

# prices_merge() --------------------------------------------------------------

test_that("prices_merge() prefers cached rows over bundled rows on key conflict", {
  local_prices()
  cache_path <- local_prices_cache()

  bundled <- prices_data
  first_row <- bundled[1, ]
  first_row$input <- 9999

  saveRDS(first_row, cache_path)
  prices_merge()

  result_row <- the$prices[
    the$prices$provider == first_row$provider &
      the$prices$model == first_row$model &
      the$prices$variant == first_row$variant,
  ]

  expect_equal(result_row$input, 9999)
})

test_that("prices_merge() uses bundled prices when no cache exists", {
  local_prices()
  local_prices_cache()

  bundled <- prices_data
  prices_merge()

  expect_equal(the$prices, bundled)
})

# prices_update() -------------------------------------------------------------

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
