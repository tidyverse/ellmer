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

# model_update_prices() aborts when download fails

    Code
      model_update_prices()
    Condition
      Error in `model_update_prices()`:
      ! Failed to download pricing data from GitHub.

