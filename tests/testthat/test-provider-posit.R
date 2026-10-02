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

test_that("set_model() swaps to the OpenAI provider for non-Claude models", {
  chat <- chat_posit(model = "claude-sonnet-4-6")
  chat$set_model("google/gemma-4-26B-A4B-it")
  provider <- chat$get_provider()
  expect_true(S7_inherits(provider, ProviderPositOpenAI))
  expect_equal(provider@base_url, "https://gateway.posit.ai/openai/v1")
  expect_equal(chat$get_model(), "google/gemma-4-26B-A4B-it")
})

test_that("set_model() swaps back to the Anthropic provider for Claude models", {
  chat <- chat_posit(model = "google/gemma-4-26B-A4B-it")
  chat$set_model("claude-sonnet-4-6")
  provider <- chat$get_provider()
  expect_true(S7_inherits(provider, ProviderPositAnthropic))
  expect_equal(provider@base_url, "https://gateway.posit.ai/anthropic/v1")
})

test_that("set_model() keeps the same provider within a family", {
  chat <- chat_posit(model = "google/gemma-4-26B-A4B-it")
  chat$set_model("zai-org/GLM-4-6")
  expect_true(S7_inherits(chat$get_provider(), ProviderPositOpenAI))

  cache_chat <- chat_posit(model = "claude-sonnet-4-6", cache = "none")
  cache_chat$set_model("claude-opus-4-6")
  expect_true(S7_inherits(cache_chat$get_provider(), ProviderPositAnthropic))
  expect_equal(cache_chat$get_provider()@cache, "none")
})

test_that("set_model() carries over credentials and headers", {
  headers <- c("X-Test" = "yes")
  credentials <- function() list(Authorization = "Bearer test")
  chat <- chat_posit(
    model = "claude-sonnet-4-6",
    credentials = credentials,
    api_headers = headers
  )
  chat$set_model("google/gemma-4-26B-A4B-it")
  provider <- chat$get_provider()
  expect_equal(provider@extra_headers, headers)
  expect_equal(provider@credentials, credentials)
})

test_that("set_model() preserves cache across model family switches", {
  chat <- chat_posit(model = "claude-sonnet-4-6", cache = "1h")
  chat$set_model("google/gemma-4-26B-A4B-it")
  expect_equal(chat$get_provider()@cache, "1h")

  chat$set_model("claude-sonnet-4-6")
  expect_equal(chat$get_provider()@cache, "1h")

  chat$set_model("claude-opus-4-6")
  expect_equal(chat$get_provider()@cache, "1h")
})

test_that("set_model() defaults cache when arriving at Claude", {
  chat <- chat_posit(model = "google/gemma-4-26B-A4B-it")
  chat$set_model("claude-sonnet-4-6")
  expect_equal(chat$get_provider()@cache, "5m")

  explicit <- chat_posit(
    model = "google/gemma-4-26B-A4B-it",
    cache = "none"
  )
  explicit$set_model("claude-sonnet-4-6")
  expect_equal(explicit$get_provider()@cache, "none")
})

test_that("unsigned thinking is replayed as text after switching to Claude", {
  # The specific models don't matter, only that they resolve to the
  # OpenAI-compatible and Anthropic provider paths; no API is called.
  chat <- chat_posit(model = "google/gemma-4-26B-A4B-it")
  chat$set_turns(list(
    UserTurn("Hi"),
    AssistantTurn(list(
      ContentThinking("Internal reasoning.", extra = list(reasoning = "raw")),
      ContentText("Hello!")
    ))
  ))
  chat$set_model("claude-sonnet-4-6")

  turns_json <- as_json(chat$get_provider(), chat$get_turns())
  content <- turns_json[[2]]$content
  expect_equal(content[[1]]$type, "text")
  expect_match(
    content[[1]]$text,
    "<thinking>\nInternal reasoning.\n</thinking>"
  )
  expect_equal(content[[2]], list(type = "text", text = "Hello!"))
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
    status_code = 403L,
    body = list(error_type = "prism_account_not_found")
  )
  expect_match(
    paste(posit_error_body(agreement), collapse = " "),
    "service agreement"
  )

  other <- response_json(
    status_code = 400L,
    body = list(error = list(message = "bad request"))
  )
  expect_equal(posit_error_body(other), "bad request")

  string_error <- response_json(
    status_code = 400L,
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
