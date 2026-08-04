# prices() --------------------------------------------------------------------

test_that("prices() prefers cached rows over bundled rows on key conflict", {
  local_prices()
  cache_path <- local_prices_cache()

  bundled_row <- prices_data[
    prices_data$provider == "OpenAI" &
      prices_data$model == "gpt-4o" &
      prices_data$variant == "",
  ]
  stopifnot("expected bundled OpenAI/gpt-4o row" = nrow(bundled_row) == 1)

  cached <- bundled_row
  cached$input <- 9999
  attr(cached, "schema_version") <- attr(prices_data, "schema_version")
  saveRDS(cached, cache_path)

  result <- prices()
  result_row <- result[
    result$provider == "OpenAI" &
      result$model == "gpt-4o" &
      result$variant == "",
  ]
  expect_equal(result_row$input, 9999)
})

test_that("prices() informs and falls back when cache schema version is older", {
  local_prices()
  cache_path <- local_prices_cache()

  stale <- prices_data
  attr(stale, "schema_version") <- attr(prices_data, "schema_version") - 1L
  saveRDS(stale, cache_path)

  expect_snapshot(result <- prices())
  expect_equal(the$prices, prices_data)
})

test_that("prices() warns and falls back when cache schema version is newer", {
  local_prices()
  cache_path <- local_prices_cache()

  newer <- prices_data
  attr(newer, "schema_version") <- attr(prices_data, "schema_version") + 1L
  saveRDS(newer, cache_path)

  expect_snapshot(result <- prices())
  expect_equal(the$prices, prices_data)
})

test_that("prices() uses bundled prices when no cache exists", {
  local_prices()
  local_prices_cache()

  prices()
  expect_equal(the$prices, prices_data)
})

# models_update_prices() -------------------------------------------------------

test_that("models_update_prices() informs and returns TRUE when download succeeds", {
  local_prices_cache()
  local_mocked_bindings(prices_cache_download = function() TRUE)

  expect_snapshot(result <- models_update_prices())
  expect_true(result)
  expect_equal(the$prices, prices_data)
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
  schema_version = attr(prices_data, "schema_version"),
  data = prices_data
) {
  jsonlite::toJSON(
    list(
      schema_version = schema_version,
      min_ellmer_version = "0.4.1.9000",
      data = data
    ),
    auto_unbox = TRUE
  )
}

test_that("prices_cache_download() writes cache and returns TRUE on 200", {
  cache_path <- local_prices_cache()
  local_mocked_responses(mock_response(body = valid_envelope()))

  expect_true(prices_cache_download())
  expect_true(file.exists(cache_path))
  cached <- readRDS(cache_path)
  expect_equal(
    attr(cached, "schema_version"),
    attr(prices_data, "schema_version")
  )
})

test_that("prices_cache_download() returns FALSE when data is unchanged", {
  local_prices_cache()
  local_mocked_responses(mock_response(body = valid_envelope()))

  expect_true(prices_cache_download())
  expect_false(prices_cache_download())
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

test_that("prices_cache_download() aborts when remote schema is newer", {
  local_prices_cache()
  newer <- attr(prices_data, "schema_version") + 1L
  local_mocked_responses(mock_response(
    body = valid_envelope(schema_version = newer)
  ))

  expect_snapshot(prices_cache_download(), error = TRUE)
})

test_that("prices_cache_download() aborts when remote schema is older", {
  local_prices_cache()
  older <- attr(prices_data, "schema_version") - 1L
  local_mocked_responses(mock_response(
    body = valid_envelope(schema_version = older)
  ))

  expect_snapshot(prices_cache_download(), error = TRUE)
})

test_that("prices_cache_download() aborts when data is missing required columns", {
  local_prices_cache()
  bad <- prices_data[, c("provider", "model", "variant")]
  local_mocked_responses(mock_response(body = valid_envelope(data = bad)))

  expect_snapshot(prices_cache_download(), error = TRUE)
})

test_that("prices_cache_download() aborts when input/output columns are non-numeric", {
  local_prices_cache()
  bad <- prices_data
  bad$input <- as.character(bad$input)
  local_mocked_responses(mock_response(body = valid_envelope(data = bad)))

  expect_snapshot(prices_cache_download(), error = TRUE)
})
