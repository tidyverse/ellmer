prices <- function() {
  if (is.null(the$prices)) {
    bundled <- prices_data
    cached <- prices_cache_read()

    cached_version <- attr(cached, "schema_version")
    bundled_version <- attr(bundled, "schema_version")
    compatible <- !is.null(cached) && identical(cached_version, bundled_version)

    if (!compatible) {
      if (!is.null(cached_version) && cached_version < bundled_version) {
        cli::cli_inform(
          c(
            "Cached pricing data uses an outdated schema.",
            i = "Run {.run ellmer::model_update_prices()} to refresh."
          ),
          .frequency = "once",
          .frequency_id = "prices_schema_mismatch"
        )
      } else if (!is.null(cached_version) && cached_version > bundled_version) {
        cli::cli_warn(
          c(
            "Cached pricing data uses a newer schema than this version of ellmer.",
            i = "Update ellmer to use the latest pricing data."
          ),
          .frequency = "once",
          .frequency_id = "prices_schema_mismatch"
        )
      }
      the$prices <- bundled
      return(invisible(the$prices))
    }

    key_cols <- c("provider", "model", "variant")
    bundled_in_cache <- !is.na(vctrs::vec_match(
      bundled[key_cols],
      cached[key_cols]
    ))
    bundled_only <- bundled[!bundled_in_cache, ]
    the$prices <- rbind(cached[names(bundled)], bundled_only)
  }

  the$prices
}

#' Update cached model pricing data
#'
#' Downloads the latest model pricing data from GitHub and saves it to the
#' local cache. Call this to refresh the prices used by [token_usage()] and
#' related functions with the latest values.
#'
#' @return Invisibly returns `TRUE` if the cache was updated, or `FALSE` if
#'   the cached data was already up to date. Throws an error if the download
#'   fails or if the \pkg{curl} package is not installed.
#' @export
model_update_prices <- function() {
  if (!requireNamespace("curl", quietly = TRUE)) {
    cli::cli_abort(
      "The {.pkg curl} package is required to update pricing data."
    )
  }
  result <- prices_cache_download()
  if (isTRUE(result)) {
    the$prices <- NULL
    cli::cli_inform(
      "Updated cached pricing data {.href [from GitHub](https://github.com/tidyverse/ellmer/blob/main/data-raw/prices.json)}."
    )
    return(invisible(TRUE))
  }
  if (isFALSE(result)) {
    cli::cli_inform("Pricing data is already up to date.")
    return(invisible(FALSE))
  }
  if (is.list(result)) {
    cli::cli_abort(
      "Pricing data on GitHub requires ellmer {result$min_ellmer_version} or later. Please update the package."
    )
  }
  cli::cli_abort("Failed to download pricing data from GitHub.")
}

prices_cache_read <- function() {
  path <- prices_cache_path()
  if (!file.exists(path)) {
    return(NULL)
  }
  cached <- tryCatch(readRDS(path), error = function(cnd) NULL)
  attr(cached, "etag") <- NULL
  cached
}

prices_cache_download <- function() {
  url <- "https://raw.githubusercontent.com/tidyverse/ellmer/refs/heads/main/data-raw/prices.json"

  handle <- curl::new_handle(
    connecttimeout_ms = 1000L,
    timeout_ms = 3000L
  )

  etag <- prices_cache_etag()
  if (!is.null(etag)) {
    curl::handle_setheaders(handle, `If-None-Match` = etag)
  }

  resp <- tryCatch(
    curl::curl_fetch_memory(url, handle = handle),
    error = function(e) NULL
  )
  if (is.null(resp)) {
    return(NA)
  }
  if (resp$status_code == 304L) {
    return(FALSE)
  }
  if (resp$status_code != 200L) {
    return(NA)
  }

  parsed <- tryCatch(
    jsonlite::fromJSON(rawToChar(resp$content)),
    error = function(e) NULL
  )

  if (!is.list(parsed) || !is.data.frame(parsed$data)) {
    return(NA)
  }

  schema_version <- as.integer(parsed$schema_version)
  if (!isTRUE(schema_version == attr(prices_data, "schema_version"))) {
    return(list(
      status = "version_mismatch",
      min_ellmer_version = parsed$min_ellmer_version
    ))
  }

  df <- parsed$data

  required <- c("provider", "model", "variant", "input", "output")
  if (!all(required %in% names(df))) {
    return(NA)
  }

  if (!is.numeric(df$input) || !is.numeric(df$output)) {
    return(NA)
  }

  attr(df, "schema_version") <- schema_version
  attr(df, "etag") <- curl::parse_headers_list(resp$headers)[["etag"]]

  path <- prices_cache_path()
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(df, path)
  TRUE
}

prices_cache_etag <- function() {
  path <- prices_cache_path()
  if (!file.exists(path)) {
    return(NULL)
  }
  cached <- tryCatch(readRDS(path), error = function(cnd) NULL)
  attr(cached, "etag")
}

prices_cache_path <- function() {
  cache_dir <- the$prices_cache_dir
  if (is.null(cache_dir)) {
    cache_dir <- normalizePath(
      tools::R_user_dir("ellmer", which = "cache"),
      mustWork = FALSE,
      winslash = "/"
    )
  }
  file.path(cache_dir, "prices.rds")
}
