# Common provider interface -----------------------------------------------

test_that("defaults are reported", {
  expect_snapshot(. <- chat_groq())
})

test_that("supports tool calling", {
  chat_fun <- chat_groq
  test_tools_simple(chat_fun)
})

# TODO: Structured data extraction test requires fixing ProviderGroq Turn
# serialization for ContentJson (currently only handles ContentText).
# test_that("can extract data", {
#   chat_fun <- function(...) chat_groq(model = "openai/gpt-oss-20b", ...)
#   test_data_extraction(chat_fun)
# })
