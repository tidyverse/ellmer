prices <- function() {
  if (is.null(the$prices)) {
    bundled <- prices_data
    cached <- prices_cache_read()

    if (is.null(cached)) {
      the$prices <- bundled
      return(invisible(the$prices))
    }

    if (is.null(cached$variant)) {
      cached$variant <- ""
    }

    key_cols <- c("provider", "model", "variant")
    bundled_in_cache <- !is.na(vctrs::vec_match(
      bundled[key_cols],
      cached[key_cols]
    ))
    bundled_only <- bundled[!bundled_in_cache, ]
    # Align columns before rbind in case cached RDS is from an older schema
    cached <- cached[, intersect(names(bundled), names(cached)), drop = FALSE]
    for (col in setdiff(names(bundled), names(cached))) {
      cached[[col]] <- NA_real_
    }
    merged <- rbind(cached[names(bundled)], bundled_only)
    the$prices <- merged
  }

  the$prices
}

prices_cache_read <- function() {
  path <- prices_cache_path()
  if (!file.exists(path)) {
    return(NULL)
  }
  tryCatch(readRDS(path), error = function(cnd) NULL)
}

prices_cache_stale <- function() {
  path <- prices_cache_path()
  if (!file.exists(path)) {
    return(TRUE)
  }
  age <- difftime(Sys.time(), file.mtime(path), units = "days")
  as.numeric(age) > 7
}

prices_should_update <- function() {
  if (is_testing()) {
    return(FALSE)
  }

  opt <- getOption("ellmer.update_prices", NULL)
  if (!is.null(opt)) {
    if (!is.na(as.logical(opt))) {
      return(as.logical(opt))
    } else {
      cli::cli_warn(
        "Option {.code ellmer.update_prices} should be set to a logical value (TRUE or FALSE).",
        call = NULL
      )
    }
  }

  env <- Sys.getenv("ELLMER_UPDATE_PRICES", "")
  if (nzchar(env)) {
    if (tolower(env) %in% c("true", "1")) {
      return(TRUE)
    } else if (tolower(env) %in% c("false", "0")) {
      return(FALSE)
    } else {
      cli::cli_warn(
        "Environment variable {.code ELLMER_UPDATE_PRICES} should be set to a logical value (true, false, 1, or 0).",
        call = NULL
      )
    }
  }

  NULL
}

prices_update <- function() {
  # prices_should_update() returns TRUE (forced on), FALSE (opted out), or NULL
  # (default). NULL means "update only if stale"; TRUE forces even if fresh.
  should_update <- prices_should_update()
  if (isFALSE(should_update)) {
    return(invisible())
  }
  if (!prices_cache_stale() && !isTRUE(should_update)) {
    return(invisible())
  }

  # Only attempt one download per session to avoid repeated pauses when offline
  if (isTRUE(the$prices_download_attempted) && !isTRUE(should_update)) {
    return(invisible())
  }
  the$prices_download_attempted <- TRUE

  downloaded <- tryCatch(prices_cache_download(), error = function(cnd) FALSE)
  if (isTRUE(downloaded)) {
    the$prices <- NULL
    cli::cli_inform(
      "Updated cached pricing data {.href [from GitHub](https://github.com/tidyverse/ellmer/blob/main/data-raw/prices.json)}."
    )
  }

  invisible()
}

prices_cache_download <- function() {
  if (!requireNamespace("curl", quietly = TRUE)) {
    return(FALSE)
  }

  url <- "https://raw.githubusercontent.com/tidyverse/ellmer/refs/heads/main/data-raw/prices.json"

  handle <- curl::new_handle(
    connecttimeout_ms = 1000L,
    timeout_ms = 3000L
  )
  resp <- tryCatch(
    curl::curl_fetch_memory(url, handle = handle),
    error = function(e) NULL
  )
  if (is.null(resp) || resp$status_code != 200L) {
    return(FALSE)
  }
  df <- tryCatch(
    jsonlite::fromJSON(rawToChar(resp$content)),
    error = function(e) NULL
  )

  if (!is.data.frame(df)) {
    return(FALSE)
  }

  required <- c("provider", "model", "variant", "input", "output")
  if (!all(required %in% names(df))) {
    return(FALSE)
  }

  if (!is.numeric(df$input) || !is.numeric(df$output)) {
    return(FALSE)
  }

  path <- prices_cache_path()
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(df, path)
  TRUE
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

local_prices_cache <- function(frame = parent.frame()) {
  cache_dir <- withr::local_tempdir(.local_envir = frame)
  old_dir <- the$prices_cache_dir
  old_prices <- the$prices
  old_attempted <- the$prices_download_attempted
  the$prices_cache_dir <- cache_dir
  the$prices <- NULL
  the$prices_download_attempted <- FALSE
  defer(
    {
      the$prices_cache_dir <- old_dir
      the$prices <- old_prices
      the$prices_download_attempted <- old_attempted
    },
    envir = frame
  )
  invisible(file.path(cache_dir, "prices.rds"))
}
