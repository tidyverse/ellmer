test_that("can make simple batch request", {
  chat <- chat_databricks(
    system_prompt = "Be as terse as possible; no punctuation"
  )
  resp <- chat$chat("What is 1 + 1?", echo = FALSE)
  expect_match(resp, "2")
  expect_equal(chat$last_turn()@tokens > 0, c(TRUE, TRUE))
})

test_that("can make simple streaming request", {
  chat <- chat_databricks(
    system_prompt = "Be as terse as possible; no punctuation"
  )
  resp <- coro::collect(chat$stream("What is 1 + 1?"))
  expect_match(paste0(unlist(resp), collapse = ""), "2")
})

# Common provider interface -----------------------------------------------

test_that("defaults are reported", {
  # Setting a dummy host ensures we don't skip this test, even if there are no
  # Databricks credentials available.
  withr::local_envvar(DATABRICKS_HOST = "https://example.cloud.databricks.com")
  expect_snapshot(. <- chat_databricks())
})

test_that("all tool variations work", {
  # Note: Databricks models cannot yet handle "continuing past the first tool
  # call", which causes issues with how ellmer implements tool calling. Nor do
  # they support parallel tool calls.
  #
  # See: https://docs.databricks.com/en/machine-learning/model-serving/function-calling.html#limitations
  # test_tools_simple(chat_databricks)
  # test_tools_async(chat_databricks)
  # test_tools_parallel(chat_databricks)
  # test_tools_sequential(chat_databricks, total_calls = 6)
})

test_that("can extract data", {
  test_data_extraction(chat_databricks)
})

test_that("can use images", {
  # Databricks models don't support images.
  #
  # test_images_inline(chat_databricks)
  # test_images_remote(chat_databricks)
})

# Auth --------------------------------------------------------------------

test_that("Databricks PATs are detected correctly", {
  withr::local_envvar(
    DATABRICKS_HOST = "https://example.cloud.databricks.com",
    DATABRICKS_TOKEN = "token"
  )
  credentials <- default_databricks_credentials()
  expect_equal(credentials(), list(Authorization = "Bearer token"))
})

test_that("Databricks CLI tokens are detected correctly", {
  withr::local_envvar(
    DATABRICKS_HOST = "https://example.cloud.databricks.com",
    DATABRICKS_CLI_PATH = "echo",
    DATABRICKS_CLIENT_ID = NA,
    DATABRICKS_CLIENT_SECRET = NA
  )
  local_mocked_bindings(databricks_cli_token = function(path, host) "cli_token")

  credentials <- default_databricks_credentials()
  expect_equal(credentials(), list(Authorization = "Bearer cli_token"))
})

test_that("M2M authentication requests look correct", {
  withr::local_envvar(
    DATABRICKS_HOST = "https://example.cloud.databricks.com",
    DATABRICKS_CLIENT_ID = "id",
    DATABRICKS_CLIENT_SECRET = "secret"
  )
  local_mocked_responses(function(req) {
    # Snapshot relevant fields of the outgoing request.
    expect_snapshot(
      list(url = req$url, headers = req$headers, body = req$body$data)
    )
    response_json(body = list(access_token = "token"))
  })
  credentials <- default_databricks_credentials()
  expect_equal(credentials(), list(Authorization = "Bearer token"))
})

test_that("workspace detection handles URLs with and without an https prefix", {
  withr::with_envvar(
    c(DATABRICKS_HOST = "example.cloud.databricks.com"),
    expect_equal(
      databricks_workspace(),
      "https://example.cloud.databricks.com"
    )
  )
  withr::with_envvar(
    c(DATABRICKS_HOST = "https://example.cloud.databricks.com"),
    expect_equal(
      databricks_workspace(),
      "https://example.cloud.databricks.com"
    )
  )
})

test_that("the user agent respects SPARK_CONNECT_USER_AGENT when set", {
  withr::with_envvar(
    c(SPARK_CONNECT_USER_AGENT = NA),
    expect_match(databricks_user_agent(), "^r-ellmer")
  )
  withr::with_envvar(
    c(SPARK_CONNECT_USER_AGENT = "testing"),
    expect_match(databricks_user_agent(), "^testing r-ellmer")
  )
})

test_that("tokens can be requested from a Connect server", {
  skip_if_not_installed("connectcreds")

  withr::local_envvar(
    DATABRICKS_HOST = "https://example.cloud.databricks.com",
    DATABRICKS_TOKEN = "token"
  )
  connectcreds::local_mocked_connect_responses(token = "token")
  credentials <- default_databricks_credentials()
  expect_equal(credentials(), list(Authorization = "Bearer token"))
})
