# Getting started --------------------------------------------------------

test_that("can make simple request", {
  chat <- chat_mistral_test("Be as terse as possible; no punctuation")
  resp <- chat$chat("What is 1 + 1?", echo = FALSE)
  expect_match(resp, "2")
  expect_equal(chat$last_turn()@tokens > 0, c(TRUE, TRUE))
})

test_that("can make simple streaming request", {
  chat <- chat_mistral_test("Be as terse as possible; no punctuation")
  resp <- coro::collect(chat$stream("What is 1 + 1?"))
  expect_match(paste0(unlist(resp), collapse = ""), "2")
})

test_that("can handle errors", {
  chat <- chat_mistral_test(model = "doesnt-exist")
  expect_snapshot(chat$chat("Hi"), error = TRUE)
})

# Common provider interface -----------------------------------------------

test_that("defaults are reported", {
  expect_snapshot(. <- chat_mistral())
})

test_that("supports standard parameters", {
  chat_fun <- chat_mistral_test

  test_params_stop(chat_fun)
})

# Tool calling is poorly supported
# test_that("all tool variations work", {
#   chat_fun <- chat_mistral_test

#   test_tools_simple(chat_fun)
#   test_tools_async(chat_fun)
#   test_tools_parallel(chat_fun)
#   test_tools_sequential(chat_fun, total_calls = 6)
# })

test_that("can extract data", {
  chat_fun <- chat_mistral_test

  test_data_extraction(chat_fun)
})

test_that("can use images", {
  chat_fun <- \(...) chat_mistral_test(model = "pixtral-12b-latest")

  test_images_inline(chat_fun)
  test_images_remote(chat_fun)
})
