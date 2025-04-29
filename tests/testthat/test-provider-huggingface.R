test_that("can make simple request", {
  chat <- chat_huggingface_test("Be as terse as possible; no punctuation")
  resp <- chat$chat("What is 1 + 1?", echo = FALSE)
  expect_match(resp, "2")
  expect_equal(chat$last_turn()@tokens > 0, c(TRUE, TRUE))
})

test_that("can make simple streaming request", {
  chat <- chat_huggingface_test("Be as terse as possible; no punctuation")
  resp <- coro::collect(chat$stream("What is 1 + 1?"))
  expect_match(paste0(unlist(resp), collapse = ""), "2")
})

# Common provider interface -----------------------------------------------

test_that("defaults are reported", {
  expect_snapshot(. <- chat_huggingface_test())
})

# Stop tokens don't appear to work correctly
# test_that("supports standard parameters", {
#   chat_fun <- chat_huggingface_test

#   test_params_stop(chat_fun)
# })

# Gets stuck in infinite loop: https://github.com/huggingface/text-generation-inference/issues/2986
# test_that("all tool variations work", {
#   chat_fun <- chat_huggingface_test

#   test_tools_simple(chat_fun)
#   test_tools_async(chat_fun)
#   test_tools_parallel(chat_fun)
#   test_tools_sequential(chat_fun, total_calls = 6)
# })

# Can't find model that does a good job
# test_that("can extract data", {
#   chat_fun <- chat_huggingface_test

#   test_data_extraction(chat_fun)
# })

# Can't find model that does a good job
# test_that("can use images", {
#   chat_fun <- function(...)
#     chat_huggingface_test(model = "Qwen/Qwen2.5-VL-7B-Instruct")

#   # Thinks hexagon is a diamond
#   test_images_inline(chat_fun, test_shape = FALSE)
#   test_images_remote(chat_fun, test_shape = FALSE)
# })
