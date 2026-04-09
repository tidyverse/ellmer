# Defaults ----------------------------------------------------------------

test_that("defaults are reported", {
  withr::local_envvar(AZURE_ANTHROPIC_API_KEY = "key")
  expect_snapshot(
    . <- chat_azure_anthropic("https://example.services.ai.azure.com/anthropic")
  )
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
    model = "claude-opus-4-5",
    params = list(),
    extra_args = list(),
    extra_headers = character(),
    credentials = \() "key",
    beta_headers = character(),
    cache = "none",
    api_version = "2024-10-22"
  )
  req <- chat_request(p, FALSE, list(turn))
  headers <- req_get_headers(req, "reveal")

  # Uses api-key header, not x-api-key like standard Anthropic
  expect_equal(headers$`api-key`, "key")
  expect_null(headers$`x-api-key`)
  # No anthropic-version header; Azure uses api-version query param instead
  expect_null(headers$`anthropic-version`)
  # api-version appears as a query parameter
  expect_match(req$url, "api-version=2024-10-22")
})

test_that("beta headers are forwarded correctly", {
  endpoint <- "https://example.services.ai.azure.com/anthropic"

  p <- ProviderAzureAnthropic(
    name = "Azure/Anthropic",
    base_url = paste0(endpoint, "/v1"),
    model = "claude-opus-4-5",
    params = list(),
    extra_args = list(),
    extra_headers = character(),
    credentials = \() "key",
    beta_headers = c("feature-a", "feature-b"),
    cache = "none",
    api_version = "2024-10-22"
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
