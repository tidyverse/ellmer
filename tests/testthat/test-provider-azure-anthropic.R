# Defaults ----------------------------------------------------------------

test_that("model is required", {
  withr::local_envvar(AZURE_ANTHROPIC_API_KEY = "key")
  expect_error(
    chat_azure_anthropic("https://example.services.ai.azure.com/anthropic"),
    "model"
  )
})

test_that("trailing slash in endpoint is handled correctly", {
  withr::local_envvar(AZURE_ANTHROPIC_API_KEY = "key")
  chat <- chat_azure_anthropic(
    endpoint = "https://example.services.ai.azure.com/anthropic/",
    model = "claude-sonnet-4-6"
  )
  expect_no_match(chat$get_provider()@base_url, "//v1")
  expect_match(chat$get_provider()@base_url, "/v1$")
})

# Authentication ----------------------------------------------------------

test_that("Azure Anthropic request headers are generated correctly", {
  turn <- UserTurn(
    contents = list(ContentText("What is 1 + 1?"))
  )
  endpoint <- "https://example.services.ai.azure.com/anthropic"

  p <- ProviderAzureAnthropic(
    name = "Azure/Anthropic",
    base_url = paste0(endpoint, "/v1"),
    model = "claude-sonnet-4-6",
    params = list(),
    extra_args = list(),
    extra_headers = character(),
    credentials = \() "key",
    beta_headers = character(),
    cache = "none"
  )
  req <- chat_request(p, FALSE, list(turn))
  headers <- req_get_headers(req, "reveal")

  # Uses x-api-key and anthropic-version, same as standard Anthropic API
  expect_equal(headers$`x-api-key`, "key")
  expect_equal(headers$`anthropic-version`, "2023-06-01")
  expect_null(headers$`api-key`)
  # No api-version query parameter
  expect_no_match(req$url, "api-version")
})

test_that("beta headers are forwarded correctly", {
  endpoint <- "https://example.services.ai.azure.com/anthropic"

  p <- ProviderAzureAnthropic(
    name = "Azure/Anthropic",
    base_url = paste0(endpoint, "/v1"),
    model = "claude-sonnet-4-6",
    params = list(),
    extra_args = list(),
    extra_headers = character(),
    credentials = \() "key",
    beta_headers = c("feature-a", "feature-b"),
    cache = "none"
  )
  req <- base_request(p)
  headers <- req_get_headers(req)
  expect_equal(headers$`anthropic-beta`, "feature-a,feature-b")
})

test_that("service principal authentication requests look correct", {
  withr::local_envvar(
    AZURE_TENANT_ID = "aaaa0a0a-bb1b-cc2c-dd3d-eeeeee4e4e4e",
    AZURE_CLIENT_ID = "id",
    AZURE_CLIENT_SECRET = "secret"
  )
  local_mocked_responses(function(req) {
    expect_snapshot(str(request_summary(req)))
    response_json(body = list(access_token = "token"))
  })
  source <- default_azure_anthropic_credentials()
  expect_equal(source(), list(Authorization = "Bearer token"))
})

test_that("tokens can be requested from a Connect server", {
  skip_if_not_installed("connectcreds")

  connectcreds::local_mocked_connect_responses(token = "token")
  credentials <- default_azure_anthropic_credentials()
  expect_equal(credentials(), list(Authorization = "Bearer token"))
})

test_that("API key is read from AZURE_ANTHROPIC_API_KEY", {
  withr::local_envvar(AZURE_ANTHROPIC_API_KEY = "my-key")
  credentials <- default_azure_anthropic_credentials()
  expect_equal(credentials(), "my-key")
})
