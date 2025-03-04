# Getting started --------------------------------------------------------

test_that("can make simple request", {
  chat <- chat_azure_test("Be as terse as possible; no punctuation")
  resp <- chat$chat("What is 1 + 1?", echo = FALSE)
  expect_match(resp, "2")
  expect_equal(chat$last_turn()@tokens > 0, c(TRUE, TRUE))
})

test_that("can make simple streaming request", {
  chat <- chat_azure_test("Be as terse as possible; no punctuation")
  resp <- coro::collect(chat$stream("What is 1 + 1?"))
  expect_match(paste0(unlist(resp), collapse = ""), "2")
})

# Common provider interface -----------------------------------------------

test_that("defaults are reported", {
  expect_snapshot(. <- chat_azure_test())
})

test_that("respects turns interface", {
  chat_fun <- chat_azure_test

  test_turns_system(chat_fun)
  test_turns_existing(chat_fun)
})

test_that("all tool variations work", {
  chat_fun <- chat_azure_test

  test_tools_simple(chat_fun)
  test_tools_async(chat_fun)
  test_tools_parallel(chat_fun)
  test_tools_sequential(chat_fun, total_calls = 6)
})

test_that("can extract data", {
  chat_fun <- chat_azure_test

  test_data_extraction(chat_fun)
})

test_that("can use images", {
  skip("Run manually; 24 hour rate limit")
  chat_fun <- chat_azure_test

  httr2::with_verbosity(test_images_inline(chat_fun), 2)
  test_images_remote(chat_fun)
})

# Authentication --------------------------------------------------------------

test_that("Azure request headers are generated correctly", {
  turn <- Turn(
    role = "user",
    contents = list(ContentText("What is 1 + 1?"))
  )
  endpoint <- "https://ai-hwickhamai260967855527.openai.azure.com"
  deployment_id <- "gpt-4o-mini"

  # API key.
  p <- ProviderAzure(
    endpoint = endpoint,
    deployment_id = deployment_id,
    api_version = "2024-06-01",
    api_key = "key",
    credentials = default_azure_credentials("key")
  )
  req <- chat_request(p, FALSE, list(turn))
  expect_snapshot(req, transform = transform_user_agent)

  # Token.
  p <- ProviderAzure(
    endpoint = endpoint,
    deployment_id = deployment_id,
    api_version = "2024-06-01",
    api_key = "",
    credentials = default_azure_credentials("", "token")
  )
  req <- chat_request(p, FALSE, list(turn))
  expect_snapshot(req, transform = transform_user_agent)

  # Both.
  p <- ProviderAzure(
    endpoint = endpoint,
    deployment_id = deployment_id,
    api_version = "2024-06-01",
    api_key = "key",
    credentials = default_azure_credentials("key", "token")
  )
  req <- chat_request(p, FALSE, list(turn))
  expect_snapshot(req, transform = transform_user_agent)
})

test_that("service principal authentication requests look correct", {
  withr::local_envvar(
    AZURE_TENANT_ID = "aaaa0a0a-bb1b-cc2c-dd3d-eeeeee4e4e4e",
    AZURE_CLIENT_ID = "id",
    AZURE_CLIENT_SECRET = "secret"
  )
  local_mocked_responses(function(req) {
    # Snapshot relevant fields of the outgoing request.
    expect_snapshot(
      list(url = req$url, headers = req$headers, body = req$body$data)
    )
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
