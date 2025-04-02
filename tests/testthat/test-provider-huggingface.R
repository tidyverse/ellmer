test_that("can make simple request", {
  chat <- chat_hf(
    "Be as terse as possible; no punctuation",
    model = "meta-llama/Llama-3.1-8B-Instruct"
  )
  resp <- chat$chat("What is 1 + 1?", echo = FALSE)
  expect_match(resp, "2")
  expect_equal(chat$last_turn()@tokens > 0, c(TRUE, TRUE))
})
