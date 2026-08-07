# Update cached model pricing data

Downloads the latest model pricing data from GitHub and saves it to the
local cache. Call this to refresh the prices used by
[`token_usage()`](https://ellmer.tidyverse.org/dev/reference/token_usage.md)
and related functions with the latest pricing data. The cache is stored
in the directory returned by
`tools::R_user_dir("ellmer", which = "cache")`.

## Usage

``` r
models_update_prices()
```

## Value

Invisibly returns `TRUE` if the cache was updated, or `FALSE` if the
cached data was already up to date. Throws an error if the download
fails.
