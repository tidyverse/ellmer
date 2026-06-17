# prices() informs and falls back when cache schema version is older

    Code
      prices()
    Message
      Cached pricing data uses an outdated schema.
      i Run `ellmer::models_update_prices()` to refresh.
      This message is displayed once per session.

# prices() warns and falls back when cache schema version is newer

    Code
      prices()
    Condition
      Warning:
      Cached pricing data uses a newer schema than this version of ellmer.
      i Update ellmer to use the latest pricing data.
      This warning is displayed once per session.

# models_update_prices() informs and returns TRUE when download succeeds

    Code
      result <- models_update_prices()
    Message
      Updated cached pricing data from GitHub (<https://github.com/tidyverse/ellmer/blob/main/data-raw/prices.json>).

# models_update_prices() informs and returns FALSE when already up to date

    Code
      result <- models_update_prices()
    Message
      Pricing data is already up to date.

# prices_cache_download() aborts on HTTP error

    Code
      prices_cache_download()
    Condition
      Error:
      ! Failed to download pricing data from GitHub (HTTP 500).

# prices_cache_download() aborts on curl error

    Code
      prices_cache_download()
    Condition
      Error:
      ! Failed to download pricing data from GitHub.
      Caused by error in `curl_fetch_memory()`:
      ! simulated network failure

# prices_cache_download() aborts on malformed JSON

    Code
      prices_cache_download()
    Condition
      Error:
      ! Failed to parse pricing data from GitHub.
      Caused by error:
      ! lexical error: invalid string in json text.
                                             not json {
                           (right here) ------^

# prices_cache_download() aborts when envelope is missing data

    Code
      prices_cache_download()
    Condition
      Error:
      ! Failed to parse pricing data from GitHub.

# prices_cache_download() aborts when remote schema is newer

    Code
      prices_cache_download()
    Condition
      Error:
      ! Pricing data on GitHub requires ellmer 0.4.1.9000 or later. Please update the package.

# prices_cache_download() aborts when remote schema is older

    Code
      prices_cache_download()
    Condition
      Error:
      ! Pricing data on GitHub uses an older schema (version 0) than this version of ellmer (version 1).
      i This usually means `main` hasn't caught up with a recent schema change.

# prices_cache_download() aborts when data is missing required columns

    Code
      prices_cache_download()
    Condition
      Error:
      ! Pricing data from GitHub is missing required columns.

# prices_cache_download() aborts when input/output columns are non-numeric

    Code
      prices_cache_download()
    Condition
      Error:
      ! Pricing data from GitHub has unexpected column types.

