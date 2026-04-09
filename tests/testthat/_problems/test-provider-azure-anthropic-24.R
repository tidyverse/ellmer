# Extracted from test-provider-azure-anthropic.R:24

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "ellmer", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
turn <- UserTurn(
    contents = list(ContentText("What is 1 + 1?"))
  )
endpoint <- "https://example.services.ai.azure.com/anthropic"
p <- ProviderAzureAnthropic(
    name = "Azure/Anthropic",
    base_url = paste0(endpoint, "/v1"),
    model = "claude-opus-4-5",
    api_version = "2024-10-22",
    credentials = \() "key"
  )
