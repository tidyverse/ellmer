# Sourceable validation seam shared by data-raw/prices.R and
# tests/testthat/test-prices-data.R. Sourcing this file only defines objects;
# it performs no network access. The expected provider set is supplied by the
# caller: data-raw/prices.R derives it from the maintained litellm lookup plus
# the hand-maintained Posit pricing; tests assert an explicit independent
# contract.

validate_price_providers <- function(prices, expected) {
  force(expected)
  actual <- sort(unique(prices$provider))
  missing <- setdiff(expected, actual)
  unexpected <- setdiff(actual, expected)

  if (length(missing) || length(unexpected)) {
    problems <- c(
      if (length(missing)) {
        c(
          "x" = "Expected provider{?s} missing from the pricing data: {.val {missing}}"
        )
      },
      if (length(unexpected)) {
        c(
          "x" = "Unexpected provider{?s} present in the pricing data: {.val {unexpected}}"
        )
      }
    )
    cli::cli_abort(
      problems,
      class = "ellmer_price_providers_invalid"
    )
  }

  invisible(actual)
}
