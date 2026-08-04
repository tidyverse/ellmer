# Model pricing data lifecycle:
#
# 1. Source of truth: `prices.json` on `main`. The
#    `.github/workflows/update-prices.yaml` workflow runs `data-raw/prices.R`
#    on a weekly schedule (and can also be run manually), which fetches from
#    litellm, validates against `data-raw/prices.schema.json`, and commits
#    `prices.json` plus the snapshot baked into `prices_data` (internal
#    package data).
#
# 2. Update: `models_update_prices()` calls `prices_cache_download()` to
#    fetch `prices.json` from GitHub and save it as an RDS at
#    `prices_cache_path()` (under `tools::R_user_dir("ellmer", "cache")`
#    by default; overridable via `the$prices_cache_dir` in tests).
#    `httr2::req_cache()` handles conditional requests, so an unchanged
#    upstream file costs at most a 304; the download is reported as an
#    update only when the parsed data differs from what's cached.
#
# 3. Read: `prices()` merges cached + bundled rows, with cached winning
#    on (provider, model, variant) conflicts. The result is memoized in
#    `the$prices`; `models_update_prices()` clears it after a refresh.
#
# Both reads and writes are gated by an integer `schema_version`; see
# `data-raw/prices.R` for the contract and when to bump it.

prices <- function() {
  if (is.null(the$prices)) {
    the$prices <- prices_merged()
  }

  the$prices
}

prices_merged <- function() {
  bundled <- prices_data
  cached <- prices_cache_read()

  if (!prices_cache_compatible(cached, bundled)) {
    return(bundled)
  }

  key_cols <- c("provider", "model", "variant")
  stopifnot(
    "cached pricing data is missing columns from bundled data" = all(
      names(bundled) %in% names(cached)
    )
  )
  bundled_in_cache <- !is.na(
    vctrs::vec_match(bundled[key_cols], cached[key_cols])
  )
  bundled_only <- bundled[!bundled_in_cache, ]
  rbind(cached[names(bundled)], bundled_only)
}

prices_cache_compatible <- function(cached, bundled) {
  cached_version <- attr(cached, "schema_version")
  bundled_version <- attr(bundled, "schema_version")

  if (!is.null(cached) && identical(cached_version, bundled_version)) {
    return(TRUE)
  }

  if (is.integer(cached_version) && length(cached_version) == 1L) {
    if (cached_version < bundled_version) {
      cli::cli_inform(
        c(
          "Cached pricing data uses an outdated schema.",
          i = "Run {.run ellmer::models_update_prices()} to refresh."
        ),
        .frequency = "once",
        .frequency_id = "prices_schema_mismatch"
      )
    } else if (cached_version > bundled_version) {
      cli::cli_warn(
        c(
          "Cached pricing data uses a newer schema than this version of ellmer.",
          i = "Update ellmer to use the latest pricing data."
        ),
        .frequency = "once",
        .frequency_id = "prices_schema_mismatch"
      )
    }
  }

  FALSE
}

#' Update cached model pricing data
#'
#' Downloads the latest model pricing data from GitHub and saves it to the
#' local cache. Call this to refresh the prices used by [token_usage()] and
#' related functions with the latest pricing data.
#'
#' @return Invisibly returns `TRUE` if the cache was updated, or `FALSE` if
#'   the cached data was already up to date. Throws an error if the download
#'   fails.
#' @export
models_update_prices <- function() {
  if (isTRUE(prices_cache_download())) {
    the$prices <- NULL
    prices()
    cli::cli_inform(
      "Updated cached pricing data {.href [from GitHub](https://github.com/tidyverse/ellmer/blob/main/data-raw/prices.json)}."
    )
    return(invisible(TRUE))
  }
  cli::cli_inform("Pricing data is already up to date.")
  invisible(FALSE)
}

prices_cache_read <- function() {
  path <- prices_cache_path()
  if (!file.exists(path)) {
    return(NULL)
  }
  tryCatch(readRDS(path), error = function(cnd) NULL)
}

prices_url <- "https://raw.githubusercontent.com/tidyverse/ellmer/refs/heads/main/data-raw/prices.json"

prices_cache_download <- function(call = caller_env()) {
  force(call)

  req <- request(prices_url)
  req <- req_cache(req, path = prices_http_cache_path())

  resp <- try_fetch(
    req_perform(req),
    error = function(cnd) {
      cli::cli_abort(
        "Failed to download pricing data from GitHub.",
        parent = cnd,
        call = call
      )
    }
  )

  # raw.githubusercontent.com serves .json as text/plain
  parsed <- try_fetch(
    resp_body_json(resp, check_type = FALSE, simplifyVector = TRUE),
    error = function(cnd) {
      cli::cli_abort(
        "Failed to parse pricing data from GitHub.",
        parent = cnd,
        call = call
      )
    }
  )

  df <- prices_check_remote(parsed, call = call)

  if (identical(df, prices_cache_read())) {
    return(FALSE)
  }

  path <- prices_cache_path()
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(df, path)
  TRUE
}

# Deliberately looser than the validation in data-raw/prices.R: this only
# needs to establish that an already-installed ellmer can read the data.
prices_check_remote <- function(parsed, call = caller_env()) {
  if (!is.list(parsed) || !is.data.frame(parsed$data)) {
    cli::cli_abort("Failed to parse pricing data from GitHub.", call = call)
  }

  remote_version <- as.integer(parsed$schema_version)
  bundled_version <- attr(prices_data, "schema_version")
  if (!isTRUE(remote_version == bundled_version)) {
    if (isTRUE(remote_version > bundled_version)) {
      cli::cli_abort(
        "Pricing data on GitHub requires ellmer {parsed$min_ellmer_version} or later. Please update the package.",
        call = call
      )
    } else {
      cli::cli_abort(
        c(
          "Pricing data on GitHub uses an older schema (version {remote_version}) than this version of ellmer (version {bundled_version}).",
          i = "This usually means {.code main} hasn't caught up with a recent schema change."
        ),
        call = call
      )
    }
  }

  df <- parsed$data

  required <- c("provider", "model", "variant", "input", "output")
  if (!all(required %in% names(df))) {
    cli::cli_abort(
      "Pricing data from GitHub is missing required columns.",
      call = call
    )
  }

  if (!is.numeric(df$input) || !is.numeric(df$output)) {
    cli::cli_abort(
      "Pricing data from GitHub has unexpected column types.",
      call = call
    )
  }

  attr(df, "schema_version") <- remote_version
  df
}

prices_cache_dir <- function() {
  the$prices_cache_dir %||%
    normalizePath(
      tools::R_user_dir("ellmer", which = "cache"),
      mustWork = FALSE,
      winslash = "/"
    )
}

prices_cache_path <- function() {
  file.path(prices_cache_dir(), "prices.rds")
}

prices_http_cache_path <- function() {
  file.path(prices_cache_dir(), "http")
}
