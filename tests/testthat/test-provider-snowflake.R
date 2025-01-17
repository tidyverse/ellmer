# Getting started --------------------------------------------------------

test_that("can make simple request", {
  # Snowflake models don't support non-streaming responses.
  #
  # chat <- chat_snowflake("Be as terse as possible; no punctuation")
  # resp <- chat$chat("What is 1 + 1?", echo = FALSE)
  # expect_match(resp, "2")
  # expect_equal(chat$last_turn()@tokens, c(64, 2))
})

test_that("can make simple streaming request", {
  chat <- chat_snowflake("Be as terse as possible; no punctuation")
  resp <- coro::collect(chat$stream("What is 1 + 1?"))
  expect_match(paste0(unlist(resp), collapse = ""), "2")
})

# Common provider interface -----------------------------------------------

test_that("defaults are reported", {
  # Setting a dummy account ensures we don't skip this test, even if there are
  # no Snowflake credentials available.
  withr::local_envvar(SNOWFLAKE_ACCOUNT = "testorg-test_account")
  credentials <- function(account) list()
  expect_snapshot(. <- chat_snowflake(credentials = credentials))
})

test_that("respects turns interface", {
  # Snowflake models don't support non-streaming responses, so these tests do
  # not yet work.
  #
  # test_turns_system(chat_snowflake)
  # test_turns_existing(chat_snowflake)
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
