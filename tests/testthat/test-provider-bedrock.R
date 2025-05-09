test_that("can make simple batch request", {
  chat <- chat_aws_bedrock("Be as terse as possible; no punctuation")
  resp <- chat$chat("What is 1 + 1?", echo = FALSE)
  expect_match(resp, "2")
  expect_equal(chat$last_turn()@tokens > 0, c(TRUE, TRUE))
})

test_that("can make simple streaming request", {
  chat <- chat_aws_bedrock("Be as terse as possible; no punctuation")
  resp <- coro::collect(chat$stream("What is 1 + 1?"))
  expect_match(paste0(unlist(resp), collapse = ""), "2")
})

test_that("can list models", {
  test_models(models_aws_bedrock)
})

test_that("can set api args", {
  chat <- chat_aws_bedrock(
    api_args = list(inferenceConfig = list(maxTokens = 1)),
    echo = FALSE
  )
  result <- chat$chat("Who are the reindeer?")
  expect_true(nchar(result) < 10)
})

test_that("handles errors", {
  chat <- chat_aws_bedrock(
    api_args = list(inferenceConfig = list(temperature = "hot")),
    echo = FALSE
  )
  expect_snapshot(error = TRUE, {
    chat$chat("What is 1 + 1?", echo = FALSE)
    chat$chat("What is 1 + 1?", echo = TRUE)
  })
})

# Common provider interface -----------------------------------------------

test_that("defaults are reported", {
  expect_snapshot(. <- chat_aws_bedrock())
})

test_that("all tool variations work", {
  chat_fun <- chat_aws_bedrock

  test_tools_simple(chat_fun)
  test_tools_async(chat_fun)
  test_tools_parallel(chat_fun)
  test_tools_sequential(chat_fun, total_calls = 6)
})

test_that("can extract data", {
  chat_fun <- chat_aws_bedrock

  test_data_extraction(chat_fun)
})

test_that("can use images", {
  chat_fun <- chat_aws_bedrock

  test_images_inline(chat_fun)
  test_images_remote_error(chat_fun)
})

test_that("can use pdfs", {
  chat_fun <- chat_aws_bedrock

  test_pdf_local(chat_fun)
})

# Provider idiosynchronies -----------------------------------------------

test_that("continues to work after whitespace only outputs (#376)", {
  chat <- chat_aws_bedrock()
  chat$chat("Respond with only two blank lines")
  expect_equal(chat$chat("What's 1+1? Just give me the number"), "2")
})

# Auth --------------------------------------------------------------------

test_that("AWS credential caching works as expected", {
  # Mock AWS credentials for different profiles.
  local_mocked_bindings(
    locate_aws_credentials = function(profile) {
      if (!is.null(profile) && profile == "test") {
        list(
          access_key_id = "key1",
          secret_key = "secret1",
          expiration = Sys.time() + 3600
        )
      } else {
        list(
          access_key_id = "key2",
          secret_key = "secret2",
          expiration = Sys.time() + 3600
        )
      }
    }
  )

  creds1 <- paws_credentials(profile = "test", reauth = TRUE)
  creds2 <- paws_credentials(profile = NULL, reauth = TRUE)

  # Verify different credentials were returned.
  expect_false(identical(creds1, creds2))
  expect_equal(creds1$access_key_id, "key1")
  expect_equal(creds2$access_key_id, "key2")

  # Verify cached credentials match original ones.
  expect_identical(creds1, paws_credentials(profile = "test"))
  expect_identical(creds2, paws_credentials(profile = NULL))

  # Simulate a cache entry that has expired.
  creds_modified <- creds1
  creds_modified$expiration <- Sys.time() - 5
  aws_creds_cache(profile = "test")$set(creds_modified)

  # Ensure the new credentials have been updated.
  expect_false(identical(creds_modified, paws_credentials(profile = "test")))
  expect_false(identical(creds1, paws_credentials(profile = "test")))
  expect_false(identical(creds2, paws_credentials(profile = "test")))
})
