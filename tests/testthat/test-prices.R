# prices_get() ----------------------------------------------------------------

test_that("prices_get() uses cached data in place of bundled data", {
  local_prices()
  cache_path <- local_prices_cache()

  df <- prices[1, ]
  df$input <- 9999
  cached <- write_prices_cache(cache_path, df)

  expect_equal(prices_get(), cached)
})

test_that("prices_get() ignores an older cached snapshot", {
  local_prices()
  cache_path <- local_prices_cache()

  df <- prices[1, ]
  df$input <- 9999
  write_prices_cache(cache_path, df, updated_at = "2000-01-01T00:00:00Z")

  expect_equal(prices_get(), prices)
})

test_that("prices_get() ignores a cache with no updated_at timestamp", {
  local_prices()
  cache_path <- local_prices_cache()

  df <- prices[1, ]
  attr(df, "schema_version") <- attr(prices, "schema_version")
  attr(df, "updated_at") <- NULL
  saveRDS(df, cache_path)

  expect_equal(prices_get(), prices)
})

test_that("prices_get() uses a newer cached snapshot", {
  local_prices()
  cache_path <- local_prices_cache()

  df <- prices[1, ]
  df$input <- 9999
  cached <- write_prices_cache(
    cache_path,
    df,
    updated_at = "2026-08-04T00:00:00Z"
  )

  expect_equal(prices_get(), cached)
})

test_that("prices_get() informs and falls back when cache schema version is older", {
  local_prices()
  cache_path <- local_prices_cache()

  older <- attr(prices, "schema_version") - 1L
  write_prices_cache(cache_path, schema_version = older)

  expect_snapshot(result <- prices_get())
  expect_equal(the$prices, prices)
})

test_that("prices_get() warns and falls back when cache schema version is newer", {
  local_prices()
  cache_path <- local_prices_cache()

  newer <- attr(prices, "schema_version") + 1L
  write_prices_cache(cache_path, schema_version = newer)

  expect_snapshot(result <- prices_get())
  expect_equal(the$prices, prices)
})

test_that("prices_get() uses bundled prices when no cache exists", {
  local_prices()
  local_prices_cache()

  prices_get()
  expect_equal(the$prices, prices)
})

test_that("prices_get() errors when cached data is missing bundled columns", {
  local_prices()
  cache_path <- local_prices_cache()

  write_prices_cache(cache_path, prices[c("provider", "model", "variant")])

  expect_snapshot(prices_get(), error = TRUE)
})

# models_update_prices() -------------------------------------------------------

test_that("models_update_prices() informs and returns TRUE when download succeeds", {
  local_prices_cache()
  local_mocked_bindings(prices_cache_download = function() TRUE)

  expect_snapshot(result <- models_update_prices())
  expect_true(result)
  expect_equal(the$prices, prices)
})

test_that("models_update_prices() informs and returns FALSE when already up to date", {
  local_prices_cache()
  local_mocked_bindings(prices_cache_download = function() FALSE)

  expect_snapshot(result <- models_update_prices())
  expect_false(result)
})

# prices_cache_download() ------------------------------------------------------

mock_response <- function(status_code = 200L, body = "") {
  resp <- response(
    status_code = status_code,
    body = charToRaw(as.character(body))
  )
  function(req) resp
}

valid_envelope <- function(
  schema_version = attr(prices, "schema_version"),
  updated_at = attr(prices, "updated_at"),
  data = prices
) {
  jsonlite::toJSON(
    list(
      schema_version = schema_version,
      min_ellmer_version = "0.5.0",
      updated_at = updated_at,
      data = data
    ),
    auto_unbox = TRUE,
    digits = 6
  )
}

test_that("prices_cache_download() skips a snapshot matching bundled data", {
  cache_path <- local_prices_cache()
  local_mocked_responses(mock_response(body = valid_envelope()))

  expect_false(prices_cache_download())
  expect_false(file.exists(cache_path))
})

test_that("prices_cache_download() writes a newer snapshot", {
  cache_path <- local_prices_cache()
  newer <- prices
  newer$input[[1]] <- newer$input[[1]] + 1
  local_mocked_responses(mock_response(
    body = valid_envelope(
      updated_at = "2026-08-04T00:00:00Z",
      data = newer
    )
  ))

  expect_true(prices_cache_download())
  expect_true(file.exists(cache_path))
  cached <- readRDS(cache_path)
  expect_equal(
    attr(cached, "schema_version"),
    attr(prices, "schema_version")
  )
  expect_equal(
    attr(cached, "updated_at"),
    "2026-08-04T00:00:00Z"
  )
})

test_that("prices_cache_download() returns FALSE when data is unchanged", {
  local_prices_cache()
  newer <- prices
  newer$input[[1]] <- newer$input[[1]] + 1
  local_mocked_responses(mock_response(
    body = valid_envelope(
      updated_at = "2026-08-04T00:00:00Z",
      data = newer
    )
  ))

  expect_true(prices_cache_download())
  expect_false(prices_cache_download())
})

test_that("prices_cache_download() leaves an older cache when bundled is current", {
  cache_path <- local_prices_cache()
  old <- prices
  old$input[[1]] <- old$input[[1]] + 1
  cached <- write_prices_cache(
    cache_path,
    old,
    updated_at = "2000-01-01T00:00:00Z"
  )
  local_mocked_responses(mock_response(body = valid_envelope()))

  expect_false(prices_cache_download())
  expect_identical(readRDS(cache_path), cached)
  expect_identical(prices_get(), prices)
})

test_that("prices_cache_download() does not replace a newer cache", {
  cache_path <- local_prices_cache()
  newer <- prices
  newer$input[[1]] <- newer$input[[1]] + 1
  cached <- write_prices_cache(
    cache_path,
    newer,
    updated_at = "2026-08-05T00:00:00Z"
  )

  remote <- prices
  remote$input[[1]] <- remote$input[[1]] + 2
  local_mocked_responses(mock_response(
    body = valid_envelope(
      updated_at = "2026-08-04T00:00:00Z",
      data = remote
    )
  ))

  expect_false(prices_cache_download())
  expect_identical(readRDS(cache_path), cached)
})

test_that("prices_cache_download() aborts on HTTP error", {
  local_prices_cache()
  local_mocked_responses(mock_response(500L))

  expect_snapshot(prices_cache_download(), error = TRUE)
})

test_that("prices_cache_download() aborts on network failure", {
  local_prices_cache()
  local_mocked_responses(function(req) {
    abort("simulated network failure", class = "httr2_failure")
  })

  expect_snapshot(prices_cache_download(), error = TRUE)
})

test_that("prices_cache_download() aborts on malformed JSON", {
  local_prices_cache()
  local_mocked_responses(mock_response(body = "not json {"))

  expect_snapshot(prices_cache_download(), error = TRUE)
})

test_that("prices_cache_download() aborts when envelope is missing data", {
  local_prices_cache()
  local_mocked_responses(mock_response(
    body = jsonlite::toJSON(list(schema_version = 1L), auto_unbox = TRUE)
  ))

  expect_snapshot(prices_cache_download(), error = TRUE)
})

test_that("prices_cache_download() aborts when updated_at is missing", {
  local_prices_cache()
  envelope <- list(
    schema_version = attr(prices, "schema_version"),
    min_ellmer_version = "0.5.0",
    data = prices
  )
  local_mocked_responses(mock_response(
    body = jsonlite::toJSON(envelope, auto_unbox = TRUE)
  ))

  expect_snapshot(prices_cache_download(), error = TRUE)
})

test_that("prices_cache_download() aborts when updated_at is invalid", {
  local_prices_cache()
  local_mocked_responses(mock_response(
    body = valid_envelope(updated_at = "2026-02-29T00:00:00Z")
  ))

  expect_snapshot(prices_cache_download(), error = TRUE)
})

test_that("prices_cache_download() aborts when remote schema is newer", {
  local_prices_cache()
  newer <- attr(prices, "schema_version") + 1L
  local_mocked_responses(mock_response(
    body = valid_envelope(schema_version = newer)
  ))

  expect_snapshot(
    prices_cache_download(),
    error = TRUE,
    transform = function(x) {
      sub("([0-9]+\\.?){3,4}", "x.y.z", x)
    }
  )
})

test_that("prices_cache_download() aborts when remote schema is older", {
  local_prices_cache()
  older <- attr(prices, "schema_version") - 1L
  local_mocked_responses(mock_response(
    body = valid_envelope(schema_version = older)
  ))

  expect_snapshot(prices_cache_download(), error = TRUE)
})

test_that("prices_cache_download() aborts when data is missing required columns", {
  local_prices_cache()
  bad <- prices[, c("provider", "model", "variant")]
  local_mocked_responses(mock_response(body = valid_envelope(data = bad)))

  expect_snapshot(prices_cache_download(), error = TRUE)
})

test_that("prices_cache_download() aborts when input/output columns are non-numeric", {
  local_prices_cache()
  bad <- prices
  bad$input <- as.character(bad$input)
  local_mocked_responses(mock_response(body = valid_envelope(data = bad)))

  expect_snapshot(prices_cache_download(), error = TRUE)
})
