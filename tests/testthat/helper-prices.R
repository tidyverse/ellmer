local_prices <- function(frame = parent.frame()) {
  old <- the$prices
  the$prices <- NULL
  defer(the$prices <- old, envir = frame)
}

write_prices_cache <- function(
  path,
  df = prices,
  schema_version = attr(prices, "schema_version"),
  updated_at = attr(prices, "updated_at")
) {
  attr(df, "schema_version") <- schema_version
  attr(df, "updated_at") <- updated_at
  saveRDS(df, path)
  invisible(df)
}

local_prices_cache <- function(frame = parent.frame()) {
  cache_dir <- withr::local_tempdir(.local_envir = frame)
  old_dir <- the$prices_cache_dir
  old_prices <- the$prices
  the$prices_cache_dir <- cache_dir
  the$prices <- NULL
  defer(
    {
      the$prices_cache_dir <- old_dir
      the$prices <- old_prices
    },
    envir = frame
  )
  invisible(file.path(cache_dir, "prices.rds"))
}
