test_that("can make simple batch request", {
  chat <- chat_bedrock("Be as terse as possible; no punctuation")
  resp <- chat$chat("What is 1 + 1?", echo = FALSE)
  expect_match(resp, "2")
  expect_equal(chat$last_turn()@tokens > 0, c(TRUE, TRUE))
})

test_that("can make simple streaming request", {
  chat <- chat_bedrock("Be as terse as possible; no punctuation")
  resp <- coro::collect(chat$stream("What is 1 + 1?"))
  expect_match(paste0(unlist(resp), collapse = ""), "2")
})

# Common provider interface -----------------------------------------------

test_that("defaults are reported", {
  expect_snapshot(. <- chat_bedrock())
})

test_that("respects turns interface", {
  chat_fun <- chat_bedrock

  test_turns_system(chat_fun)
  test_turns_existing(chat_fun)
})

test_that("all tool variations work", {
  chat_fun <- chat_bedrock

  test_tools_simple(chat_fun)
  test_tools_async(chat_fun)
  test_tools_parallel(chat_fun)
  test_tools_sequential(chat_fun, total_calls = 6)
})

test_that("can extract data", {
  chat_fun <- chat_bedrock

  test_data_extraction(chat_fun)
})

test_that("can use images", {
  chat_fun <- chat_bedrock

  test_images_inline(chat_fun)
  test_images_remote_error(chat_fun)
})

test_that("can use pdfs", {
  chat_fun <- chat_bedrock

  test_pdf_local(chat_fun)
})


# Auth --------------------------------------------------------------------

test_that("AWS credential caching works as expected", {
  # Mock AWS credentials for different profiles.
  local_mocked_bindings(
    locate_aws_credentials = function(profile) {
      if (!is.null(profile) && profile == "test") {
        list(
          access_key = "key1",
          secret_key = "secret1",
          expiration = Sys.time() + 3600
        )
      } else {
        list(
          access_key = "key2",
          secret_key = "secret2",
          expiration = Sys.time() + 3600
        )
      }
    }
  )

  creds1 <- paws_credentials(profile = "test")
  creds2 <- paws_credentials(profile = NULL)

  # Verify different credentials were returned.
  expect_false(identical(creds1, creds2))
  expect_equal(creds1$access_key, "key1")
  expect_equal(creds2$access_key, "key2")

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
