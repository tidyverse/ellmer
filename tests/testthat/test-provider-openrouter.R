# Getting started --------------------------------------------------------

test_that("can make simple request", {
  chat <- chat_openrouter_test("Be as terse as possible; no punctuation")
  resp <- chat$chat("What is 1 + 1?", echo = FALSE)
  expect_match(resp, "2")
  expect_equal(unname(chat$last_turn()@tokens[1:2] > 0), c(TRUE, TRUE))
})

test_that("can make simple streaming request", {
  chat <- chat_openrouter_test("Be as terse as possible; no punctuation")
  resp <- coro::collect(chat$stream("What is 1 + 1?"))
  expect_match(paste0(unlist(resp), collapse = ""), "2")
})

test_that("handles errors", {
  chat <- chat_openrouter_test(api_args = list(temperature = "hot"))
  expect_snapshot(error = TRUE, {
    chat$chat("What is 1 + 1?", echo = FALSE)
    chat$chat("What is 1 + 1?", echo = TRUE)
  })
})

# Authentication ----------------------------------------------------------

test_that("login url only includes callback and state when supplied", {
  pkce <- list(challenge = "challenge", method = "S256")
  expect_snapshot({
    openrouter_auth_url(pkce)
    openrouter_auth_url(
      pkce,
      state = "state",
      callback_url = "http://localhost:1410/"
    )
  })
})

test_that("redirect uri gets a port only when it has none", {
  withr::local_envvar(HTTR2_OAUTH_REDIRECT_URL = NA)
  expect_match(url_parse(openrouter_redirect_uri())$port, "^[0-9]+$")

  withr::local_envvar(HTTR2_OAUTH_REDIRECT_URL = "http://localhost:1410/")
  expect_equal(openrouter_redirect_uri(), "http://localhost:1410/")
})

test_that("exchanges the code for a key", {
  local_mocked_responses(function(req) {
    expect_snapshot(str(request_summary(req)))
    response_json(body = list(key = "sk-or-test"))
  })
  token <- openrouter_exchange_code("code", "verifier")
  expect_equal(token$access_token, "sk-or-test")
})

test_that("default credentials prefer the env var, then the cache, then a login", {
  withr::local_envvar(
    OPENROUTER_API_KEY = "abc",
    HTTR2_OAUTH_CACHE = withr::local_tempdir()
  )
  expect_equal(default_openrouter_credentials()(), "abc")

  withr::local_envvar(OPENROUTER_API_KEY = NA)
  local_mocked_bindings(is_testing = function() FALSE)
  expect_snapshot(default_openrouter_credentials(), error = TRUE)

  withr::local_options(rlang_interactive = TRUE)
  expect_type(default_openrouter_credentials(), "closure")

  withr::local_options(rlang_interactive = FALSE)
  cache_dir <- file.path(
    httr2::oauth_cache_path(),
    openrouter_oauth_client()$name
  )
  dir.create(cache_dir, recursive = TRUE)
  file.create(file.path(cache_dir, "fake-token.rds.enc"))
  expect_type(default_openrouter_credentials(), "closure")
})

local_cached_openrouter_key <- function(key, frame = caller_env()) {
  withr::local_envvar(
    OPENROUTER_API_KEY = NA,
    HTTR2_OAUTH_CACHE = withr::local_tempdir(.local_envir = frame),
    .local_envir = frame
  )
  suppressMessages(oauth_token_cached(
    openrouter_oauth_client(),
    \(client) oauth_token(key),
    cache_disk = TRUE
  ))
}

test_that("a cached key is checked once and then reused", {
  local_cached_openrouter_key("sk-or-cached")
  checks <- 0
  local_mocked_responses(function(req) {
    checks <<- checks + 1
    response_json(body = list(data = list(label = "ellmer")))
  })
  credentials <- default_openrouter_credentials()
  expect_equal(credentials(), "sk-or-cached")
  expect_equal(credentials(), "sk-or-cached")
  expect_equal(checks, 1)
})

test_that("a revoked cached key is cleared and the user logs in again", {
  local_cached_openrouter_key("sk-or-revoked")
  local_mocked_responses(function(req) response(status_code = 401L))

  expect_snapshot(default_openrouter_credentials()(), error = TRUE)
  expect_equal(openrouter_oauth_has_cache(openrouter_oauth_client()), FALSE)

  local_cached_openrouter_key("sk-or-revoked")
  withr::local_options(rlang_interactive = TRUE)
  local_mocked_bindings(
    openrouter_oauth_flow = function(client, ...) oauth_token("sk-or-fresh")
  )
  expect_equal(
    suppressMessages(default_openrouter_credentials()()),
    "sk-or-fresh"
  )
})

# Common provider interface -----------------------------------------------

test_that("supports tool calling", {
  chat_fun <- chat_openrouter_test

  test_tools_simple(chat_fun)
})

test_that("can extract data", {
  chat_fun <- chat_openrouter_test

  test_data_extraction(chat_fun)
})

test_that("can use images", {
  chat_fun <- chat_openrouter_test

  test_images_inline(chat_fun)
  test_images_remote(chat_fun)
})
