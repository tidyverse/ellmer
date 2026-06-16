test_that("uses the Anthropic API for Claude models", {
  chat <- chat_posit(model = "claude-sonnet-4-6")
  provider <- chat$get_provider()
  expect_true(S7_inherits(provider, ProviderPositAnthropic))
  expect_equal(provider@base_url, "https://gateway.posit.ai/anthropic/v1")
})

test_that("uses the OpenAI-compatible API for other models", {
  chat <- chat_posit(model = "google/gemma-4-26B-A4B-it")
  provider <- chat$get_provider()
  expect_true(S7_inherits(provider, ProviderPositOpenAI))
  expect_equal(provider@base_url, "https://gateway.posit.ai/openai/v1")
})

test_that("can derive the gateway url from a flavored base url", {
  expect_equal(
    posit_gateway_url("https://gateway.posit.ai/anthropic/v1"),
    "https://gateway.posit.ai"
  )
  expect_equal(
    posit_gateway_url("https://gateway.posit.ai/openai/v1"),
    "https://gateway.posit.ai"
  )
})

test_that("gateway-specific errors get useful messages", {
  agreement <- response_json(
    status = 403L,
    body = list(error_type = "prism_account_not_found")
  )
  expect_match(
    paste(posit_error_body(agreement), collapse = " "),
    "service agreement"
  )

  other <- response_json(
    status = 400L,
    body = list(error = list(message = "bad request"))
  )
  expect_equal(posit_error_body(other), "bad request")

  string_error <- response_json(
    status = 400L,
    body = list(error = "bad request")
  )
  expect_equal(posit_error_body(string_error), "bad request")
})

# Checking the cache before calling models_posit() keeps an unauthenticated
# machine from triggering (and hanging on) the interactive device flow.
available_posit_models <- function() {
  skip_if_offline()
  cache_dir <- file.path(httr2::oauth_cache_path(), posit_oauth_client()$name)
  if (length(dir(cache_dir, pattern = "token")) == 0) {
    skip("not authenticated with Posit AI")
  }
  tryCatch(
    models_posit()$id,
    error = function(cnd) skip("could not list Posit AI models")
  )
}

test_that("supports tool calling with Claude models", {
  model <- "claude-sonnet-4-6"
  skip_if_not(model %in% available_posit_models())
  test_tools_simple(\(...) chat_posit(model = model, ...))
})

test_that("supports tool calling with Gemma models", {
  model <- "google/gemma-4-26B-A4B-it"
  skip_if_not(model %in% available_posit_models())
  test_tools_simple(\(...) chat_posit(model = model, ...))
})
