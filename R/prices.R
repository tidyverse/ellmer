prices <- function() {
  if (is.null(the$prices)) {
    prices_merge()
  }
  the$prices
}

prices_merge <- function() {
  bundled <- prices_data
  cached <- prices_cache_read()

  if (is.null(cached)) {
    the$prices <- bundled
    return(invisible())
  }

  key_cols <- c("provider", "model", "variant")
  bundled_key <- do.call(paste, c(bundled[key_cols], sep = "\r"))
  cached_key <- do.call(paste, c(cached[key_cols], sep = "\r"))

  bundled_only <- bundled[!bundled_key %in% cached_key, ]
  merged <- rbind(cached, bundled_only)
  the$prices <- merged[, names(bundled)]

  invisible()
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

prices_update <- function() {
  tryCatch(
    {
      if (isFALSE(getOption("ellmer.update_prices"))) {
        return(invisible())
      }
      if (identical(Sys.getenv("ELLMER_UPDATE_PRICES"), "false")) {
        return(invisible())
      }
      if (!prices_cache_stale()) {
        return(invisible())
      }

      url <- "https://raw.githubusercontent.com/tidyverse/ellmer/refs/heads/main/data-raw/prices.json"
      resp <- curl::curl_fetch_memory(url)
      df <- jsonlite::fromJSON(rawToChar(resp$content))

      if (
        !is.data.frame(df) ||
          !all(c("provider", "model", "variant") %in% names(df)) ||
          nrow(df) < 500
      ) {
        return(invisible())
      }

      path <- prices_cache_path()
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
      saveRDS(df, path)
      the$prices <- NULL
    },
    error = function(cnd) invisible()
  )

  invisible()
}

prices_cache_path <- function() {
  cache_dir <- the$prices_cache_dir %||% tools::R_user_dir("ellmer", "cache")
  file.path(cache_dir, "prices.rds")
}

local_prices_cache <- function(frame = parent.frame()) {
  cache_dir <- withr::local_tempdir(.local_envir = frame)
  old <- the$prices_cache_dir
  the$prices_cache_dir <- cache_dir
  defer(the$prices_cache_dir <- old, envir = frame)
  invisible(file.path(cache_dir, "prices.rds"))
}
