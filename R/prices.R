# Model pricing data lifecycle:
#
# 1. Source of truth: `prices.json` on `main`. The
#    `.github/workflows/update-prices.yaml` workflow runs `data-raw/prices.R`
#    on a weekly schedule (and can also be run manually), which fetches from
#    litellm, validates against `data-raw/prices.schema.json`, and commits
#    `prices.json` plus the snapshot baked into `prices` (internal package
#    data).
#
# 2. Update: `models_update_prices()` calls `prices_cache_download()` to
#    fetch `prices.json` from GitHub and save it as an RDS at
#    `prices_cache_path()` (under `tools::R_user_dir("ellmer", "cache")`
#    by default; overridable via `the$prices_cache_dir` in tests).
#    `httr2::req_cache()` handles conditional requests, so an unchanged
#    upstream file costs at most a 304; the download is reported as an
#    update only when the remote snapshot is newer than local data.
#
# 3. Read: `prices_get()` uses the cached data when it's compatible with
#    this version of ellmer, otherwise the bundled snapshot. The result is
#    memoized in `the$prices`; `models_update_prices()` clears it after a
#    refresh.
#
# Both reads and writes are gated by an integer `schema_version`; see
# `data-raw/prices.R` for the contract and when to bump it. Each snapshot also
# records when its pricing data was updated. An older cached snapshot loses to
# the bundled snapshot.

prices_get <- function() {
  if (is.null(the$prices)) {
    cached <- prices_cache_read()
    the$prices <- if (prices_cache_compatible(cached, prices)) {
      cached
    } else {
      prices
    }
  }

  the$prices
}

prices_cache_compatible <- function(cached, bundled) {
  cached_version <- attr(cached, "schema_version")
  bundled_version <- attr(bundled, "schema_version")

  if (!is.null(cached) && identical(cached_version, bundled_version)) {
    stopifnot(
      "cached pricing data is missing columns from bundled data" = all(
        names(bundled) %in% names(cached)
      )
    )
    return(!prices_snapshot_older(cached, bundled))
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

prices_snapshot_older <- function(snapshot, reference) {
  snapshot_updated_at <- attr(snapshot, "updated_at")
  reference_updated_at <- attr(reference, "updated_at")

  if (!prices_updated_at_valid(snapshot_updated_at)) {
    return(TRUE)
  }

  stopifnot(
    "bundled pricing data has an invalid updated_at timestamp" = prices_updated_at_valid(
      reference_updated_at
    )
  )
  snapshot_updated_at < reference_updated_at
}

prices_updated_at_valid <- function(x) {
  if (
    !is.character(x) ||
      length(x) != 1L ||
      is.na(x) ||
      !grepl("^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$", x)
  ) {
    return(FALSE)
  }

  parsed <- as.POSIXct(
    x,
    format = "%Y-%m-%dT%H:%M:%SZ",
    tz = "UTC"
  )
  !is.na(parsed) &&
    identical(format(parsed, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), x)
}

#' Update cached model pricing data
#'
#' Downloads the latest model pricing data from GitHub and saves it to the
#' local cache. Call this to refresh the prices used by [token_usage()] and
#' related functions with the latest pricing data. The cache is stored in the
#' directory returned by `tools::R_user_dir("ellmer", which = "cache")`.
#'
#' @return Invisibly returns `TRUE` if the cache was updated, or `FALSE` if
#'   the cached data was already up to date. Throws an error if the download
#'   fails.
#' @export
models_update_prices <- function() {
  if (isTRUE(prices_cache_download())) {
    the$prices <- NULL
    prices_get()
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

  if (
    identical(df, prices_cache_read()) ||
      !prices_snapshot_older(prices_get(), df)
  ) {
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
  bundled_version <- attr(prices, "schema_version")
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

  updated_at <- parsed$updated_at
  if (!prices_updated_at_valid(updated_at)) {
    cli::cli_abort(
      "Pricing data from GitHub has an invalid {.field updated_at} timestamp.",
      call = call
    )
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
  attr(df, "updated_at") <- updated_at
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
