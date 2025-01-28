# Getting started --------------------------------------------------------

test_that("can make simple request", {
  chat <- chat_ollama_test("Be as terse as possible; no punctuation")
  resp <- chat$chat("What is 1 + 1?", echo = FALSE)
  expect_match(resp, "2")
  expect_equal(chat$last_turn()@tokens >= 1, c(TRUE, TRUE))
})

test_that("can make simple streaming request", {
  chat <- chat_ollama_test("Be as terse as possible; no punctuation")
  resp <- coro::collect(chat$stream("What is 1 + 1?"))
  expect_match(paste0(unlist(resp), collapse = ""), "2")
})

# Common provider interface -----------------------------------------------

# Currently no other tests because I can't find a model that returns reliable
# results and is reasonably performant.
