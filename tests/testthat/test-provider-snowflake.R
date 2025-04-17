# Getting started --------------------------------------------------------

test_that("can make simple request", {
  # Snowflake models don't support non-streaming responses.
  #
  # chat <- chat_snowflake("Be as terse as possible; no punctuation")
  # resp <- chat$chat("What is 1 + 1?", echo = FALSE)
  # expect_match(resp, "2")
  # expect_equal(chat$last_turn()@tokens > 0, c(TRUE, TRUE))
})

test_that("can make simple streaming request", {
  chat <- chat_snowflake("Be as terse as possible; no punctuation")
  resp <- coro::collect(chat$stream("What is 1 + 1?"))
  expect_match(paste0(unlist(resp), collapse = ""), "2")
  expect_equal(chat$last_turn()@tokens > 0, c(TRUE, TRUE))
})

# Common provider interface -----------------------------------------------

test_that("defaults are reported", {
  # Setting a dummy account ensures we don't skip this test, even if there are
  # no Snowflake credentials available.
  withr::local_envvar(
    SNOWFLAKE_ACCOUNT = "testorg-test_account",
    SNOWFLAKE_TOKEN = "token"
  )
  expect_snapshot(. <- chat_snowflake())
})

test_that("all tool variations work", {
  # Snowflake models don't support tool calling.
  #
  # test_tools_simple(chat_snowflake)
  # test_tools_async(chat_snowflake)
  # test_tools_parallel(chat_snowflake)
  # test_tools_sequential(chat_snowflake, total_calls = 6)
})

test_that("can extract data", {
  # Snowflake models don't support structured data.
  #
  # test_data_extraction(chat_snowflake)
})

test_that("can use images", {
  # Snowflake models don't support images.
  #
  # test_images_inline(chat_snowflake)
  # test_images_remote(chat_snowflake)
})

# Auth --------------------------------------------------------------------

test_snowflake_key <- "-----BEGIN PRIVATE KEY-----
MIIEvgIBADANBgkqhkiG9w0BAQEFAASCBKgwggSkAgEAAoIBAQCbxG4OC5HU9QlK
dmtbQCa7r+uoKyDSisxqJQchfkDy64v6V6WsovI8evUGPQpkAbqsmXY3DR3T/Mco
P2oHyzGsfd2t7v6NLHNtGbMiEJYjVJvOw52Yn1m4WH5bEtl5JP/8W2qyTdr6qym+
m47X8hAqb+ToQjnolRq9xlme9n6vhwGi8mlco5POLCcEDhcYiqcPxI/WRqHDcdi8
/nU1eRhGTSe77NUnC0QQOojjRZ3P59NuA7zpgFdMLdE0I5qrfL3e6SQlnmTizdng
qiZUAI5p1ISZffn1FLf9GZlnD/usG0Dbp2MDGbYMhx8a5ii0RGyEINYrATdxkKaW
/AKGeKbdAgMBAAECggEAH+a022+HKGQeyP9DsWaMCDhZPRHIIRaIEt0Ofs+KobWX
72dv6NFeZwCPmf16WUz5XEv5qACpsTa92wJRxtLYk4kbk3m07FjEMv3mb/2Roh67
4jax2gYYq+aDykcr/uGTA639RhMn29qeLAlT0eojYW2VJfQaRAX1ehRbWnEFNRFR
Pyy1pBOCReDG0yyw1OtDv85H09UdiRWyNC7HkxdaMZns//GOQ3MwuZL61st7Aezg
Xz+mGw7v+SEIL0zk92GSIOHA0TXiUAhIWGxIyqSeNqw+Cl0+4r6ZuT+z2lILPR9C
UPVMtXUzUBhBPhtvPpq2RoRqcHzXWsdUcfteKyN/iQKBgQC5JvzZOwOnBcqaTUpn
ykrYwyiAOk0h3uOs4Mrs7A40xWmQ35VOb1gWVnTgvC91SBBfP/jGf02ZdLk5NG1/
oe13aKvQ6mh/jTImPLEPxsMm+469+nklitHwF8b6R3zrSPHoqdF4XUOHOcbK5V8W
MgUIIXDGtLCqxTns41VbIM9/5wKBgQDXXvgG5238F1LtHFG0FNZRilRt3d+cO1CU
HctSPGRXVe8ZGEJZ4F/TV6pWEOrdsuk5bp/IoDGKE2b9FI6K9BKy3Xc8qx5Um9zF
q5ca671UmZkcqu8jh99JSn9sKM7PP9QZInhP1eca7J9r2lhROHk0hsyTWtzuVcWO
JttBO0lamwKBgQCJWFGCNxO7h0FGewUxvs8MwqA9loH3GScc69e8LlNPdA2eKSzR
dSkL0PB8cTxnLKDwdzzsyixfJEXuGGUNo6nKxTuHCwufarcDxEu4H0JOnZbCeJX7
cmHPT2QL7pHM21yPscEwH0bjfcloYwPJLCutX1kQHaNb2lfg0LZVlh42iwKBgQCW
3yp0+66qiFRJUitSMb6pRHQ8us8ojMy31d9W7oOEQujJ9ZqVh37ZeHIU9KjzQZ/r
4bkBPGc3yLu+0qXAZZarwkUDNQR8VOtldfzWmQn6t9bwpDX99/LNTujQhg3KVXZp
XSJXGwtYayaK0VxJGXye9UdeeqqGM4O/Py0dF0EdvQKBgDo82ImF2mKzJUEBK33r
uGtR8Fxbg4cNRAc0W6xME86IVTnLnqLp1yeTZZGCFek6hDqERLCbQhQk8t1Szm0V
OdYSh6YfkxhsBGp6hHefOTWuoto4zHZ98uuu0GD8NkzGmnZApZ7It1MiH+SZPG9w
AK4HbizZMWlkvg87OphvnQhC
-----END PRIVATE KEY-----
"

test_that("Snowflake keypair token caching works as expected", {
  skip_if_not_installed("jose")

  # Random RSA key for testing.
  testkey <- openssl::read_key(test_snowflake_key)

  token1 <- snowflake_keypair_token("test1", "user", testkey)
  token2 <- snowflake_keypair_token("test2", "user", testkey)

  # Verify different tokens were returned
  expect_false(identical(token1, token2))

  # Verify cached tokens match original ones
  expect_identical(token1, snowflake_keypair_token("test1", "user", testkey))
  expect_identical(token2, snowflake_keypair_token("test2", "user", testkey))

  # Simulate a cache entry that has expired
  cache <- snowflake_keypair_cache("test1", testkey)
  creds_modified <- cache$get()
  creds_modified$expiry <- Sys.time() - 5
  cache$set(creds_modified)

  # Ensure the new token has been updated
  expect_false(
    identical(
      creds_modified,
      snowflake_keypair_token("test1", "user", testkey)
    )
  )
  expect_false(
    identical(token1, snowflake_keypair_token("test1", "user", testkey))
  )
  expect_false(
    identical(token2, snowflake_keypair_token("test1", "user", testkey))
  )
})

test_that("Snowflake OAuth tokens are detected correctly", {
  withr::local_envvar(
    SNOWFLAKE_ACCOUNT = "testorg-test_account",
    SNOWFLAKE_TOKEN = "token"
  )
  credentials <- default_snowflake_credentials()
  expect_identical(
    credentials(),
    list(
      Authorization = "Bearer token",
      `X-Snowflake-Authorization-Token-Type` = "OAUTH"
    )
  )
})

test_that("Snowflake key-pair credentials are detected correctly", {
  skip_if_not_installed("jose")

  withr::local_envvar(
    SNOWFLAKE_ACCOUNT = "testorg-test_account",
    SNOWFLAKE_USER = "user",
    SNOWFLAKE_PRIVATE_KEY = test_snowflake_key
  )
  # Warm the cache so we can compare more easily.
  token <- snowflake_keypair_token(
    "testorg-test_account",
    "user",
    openssl::read_key(test_snowflake_key)
  )
  credentials <- default_snowflake_credentials()
  expect_identical(
    credentials(),
    list(
      Authorization = paste("Bearer", token),
      `X-Snowflake-Authorization-Token-Type` = "KEYPAIR_JWT"
    )
  )
})

test_that("tokens can be requested from a Connect server", {
  skip_if_not_installed("connectcreds")

  withr::local_envvar(SNOWFLAKE_ACCOUNT = "testorg-test_account")
  connectcreds::local_mocked_connect_responses(token = "token")
  credentials <- default_snowflake_credentials()
  expect_identical(
    credentials(),
    list(
      Authorization = "Bearer token",
      `X-Snowflake-Authorization-Token-Type` = "OAUTH"
    )
  )
})
