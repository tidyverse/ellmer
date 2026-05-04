local_prices <- function(frame = parent.frame()) {
  old <- the$prices
  the$prices <- NULL
  defer(the$prices <- old, envir = frame)
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
