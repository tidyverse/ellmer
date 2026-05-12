# prices() informs and falls back when cache schema version is older

    Code
      prices()
    Message
      Cached pricing data uses an outdated schema.
      i Run `ellmer::model_update_prices()` to refresh.
      This message is displayed once per session.

# prices() warns and falls back when cache schema version is newer

    Code
      prices()
    Condition
      Warning:
      Cached pricing data uses a newer schema than this version of ellmer.
      i Update ellmer to use the latest pricing data.
      This warning is displayed once per session.

# model_update_prices() informs and returns TRUE when download succeeds

    Code
      result <- model_update_prices()
    Message
      Updated cached pricing data from GitHub (<https://github.com/tidyverse/ellmer/blob/main/data-raw/prices.json>).

# model_update_prices() informs and returns FALSE when already up to date

    Code
      result <- model_update_prices()
    Message
      Pricing data is already up to date.

# model_update_prices() aborts with version mismatch advice

    Code
      model_update_prices()
    Condition
      Error in `model_update_prices()`:
      ! Pricing data on GitHub requires ellmer 0.5.0 or later. Please update the package.

# model_update_prices() aborts when download fails

    Code
      model_update_prices()
    Condition
      Error in `model_update_prices()`:
      ! Failed to download pricing data from GitHub.

