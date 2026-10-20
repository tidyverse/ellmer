# Kept separate so package tests can source the provider validator directly
# without running the network-dependent price updater.

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
