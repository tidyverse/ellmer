# Getting started --------------------------------------------------------

test_that("can make simple request", {
  chat <- chat_azure_openai_test("Be as terse as possible; no punctuation")
  resp <- chat$chat("What is 1 + 1?", echo = FALSE)
  expect_match(resp, "2")
  expect_equal(chat$last_turn()@tokens[1:2] > 0, c(TRUE, TRUE))
})

test_that("can make simple streaming request", {
  chat <- chat_azure_openai_test("Be as terse as possible; no punctuation")
  resp <- coro::collect(chat$stream("What is 1 + 1?"))
  expect_match(paste0(unlist(resp), collapse = ""), "2")
})

# Common provider interface -----------------------------------------------

test_that("defaults are reported", {
  withr::local_envvar(AZURE_OPENAI_API_KEY = "key")
  expect_snapshot(. <- chat_azure_openai("endpoint", "deployment_id"))
})

test_that("supports standard parameters", {
  chat_fun <- chat_azure_openai_test

  test_params_stop(chat_fun)
})

test_that("supports tool calling", {
  chat_fun <- chat_azure_openai_test

  test_tools_simple(chat_fun)
})

test_that("can extract data", {
  chat_fun <- chat_azure_openai_test

  test_data_extraction(chat_fun)
})

test_that("can use images", {
  skip("Run manually; 24 hour rate limit")
  chat_fun <- chat_azure_openai_test

  test_images_inline(chat_fun)
  test_images_remote(chat_fun)
})

# Authentication --------------------------------------------------------------

test_that("Azure request headers are generated correctly", {
  turn <- Turn(
    role = "user",
    contents = list(ContentText("What is 1 + 1?"))
  )
  deployment_id <- "gpt-4o-mini"
  base_url <- paste0(
    "https://ai-hwickhamai260967855527.openai.azure.com/openai/deployments/",
    deployment_id
  )

  # API key.
  p <- ProviderAzureOpenAI(
    name = "Azure",
    base_url = base_url,
    model = deployment_id,
    api_version = "2024-06-01",
    api_key = "key",
    credentials = default_azure_credentials("key")
  )
  req <- chat_request(p, FALSE, list(turn))
  expect_snapshot(str(req_get_headers(req, "reveal")))

  # Token.
  p <- ProviderAzureOpenAI(
    name = "Azure",
    base_url = base_url,
    model = deployment_id,
    api_version = "2024-06-01",
    api_key = "",
    credentials = default_azure_credentials("", "token")
  )
  req <- chat_request(p, FALSE, list(turn))
  expect_snapshot(str(req_get_headers(req, "reveal")))

  # Both.
  p <- ProviderAzureOpenAI(
    name = "Azure",
    base_url = base_url,
    model = deployment_id,
    api_version = "2024-06-01",
    api_key = "key",
    credentials = default_azure_credentials("key", "token")
  )
  req <- chat_request(p, FALSE, list(turn))
  expect_snapshot(str(req_get_headers(req, "reveal")))
})

test_that("service principal authentication requests look correct", {
  withr::local_envvar(
    AZURE_TENANT_ID = "aaaa0a0a-bb1b-cc2c-dd3d-eeeeee4e4e4e",
    AZURE_CLIENT_ID = "id",
    AZURE_CLIENT_SECRET = "secret"
  )
  local_mocked_responses(function(req) {
    # Snapshot relevant fields of the outgoing request.
    expect_snapshot(str(request_summary(req)))
    response_json(body = list(access_token = "token"))
  })
  source <- default_azure_credentials()
  expect_equal(source(), list(Authorization = "Bearer token"))
})

test_that("tokens can be requested from a Connect server", {
  skip_if_not_installed("connectcreds")

  connectcreds::local_mocked_connect_responses(token = "token")
  credentials <- default_azure_credentials()
  expect_equal(credentials(), list(Authorization = "Bearer token"))
})
