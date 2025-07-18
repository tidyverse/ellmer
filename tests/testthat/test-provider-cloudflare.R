# Getting started --------------------------------------------------------

test_that("can make simple request", {
  chat <- chat_cloudflare_test("Be as terse as possible; no punctuation")
  resp <- chat$chat("What is 1 + 1?", echo = FALSE)
  expect_match(resp, "2")
  expect_equal(chat$last_turn()@tokens[1:2] > 0, c(TRUE, TRUE))
})

test_that("can make simple streaming request", {
  chat <- chat_cloudflare_test("Be as terse as possible; no punctuation")
  resp <- coro::collect(chat$stream("What is 1 + 1?"))
  expect_match(paste0(unlist(resp), collapse = ""), "2")
})

# Common provider interface -----------------------------------------------

test_that("defaults are reported", {
  expect_snapshot(. <- chat_cloudflare())
})

# Not supported
# test_that("supports standard parameters", {
#   chat_fun <- chat_cloudflare_test

#   test_params_stop(chat_fun)
# })

# Doesn't appear to work
# test_that("supports tool calling", {
#   chat_fun <- function(...) {
#     chat_cloudflare_test(
#       ...,
#       model = "@hf/nousresearch/hermes-2-pro-mistral-7b"
#     )
#   }

#   test_tools_simple(chat_fun)
# })

test_that("can extract data", {
  chat_fun <- chat_cloudflare_test

  test_data_extraction(chat_fun)
})

# Can't find model that works
# test_that("can use images", {
#   chat_fun <- function(...)
#     chat_cloudflare_test(model = "@cf/llava-hf/llava-1.5-7b-hf", ...)

#   test_images_inline(chat_fun)
#   # test_images_remote(chat_fun)
# })
