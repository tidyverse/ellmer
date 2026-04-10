# Extracted from test-provider-azure-anthropic.R:47

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "ellmer", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
endpoint <- "https://example.services.ai.azure.com/anthropic"
p <- ProviderAzureAnthropic(
  name = "Azure/Anthropic",
  base_url = paste0(endpoint, "/v1"),
  model = "claude-opus-4-5",
  api_version = "2024-10-22",
  credentials = \() "key",
  beta_headers = c("feature-a", "feature-b")
)
