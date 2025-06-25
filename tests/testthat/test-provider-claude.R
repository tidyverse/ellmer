test_that("can make simple batch request", {
  chat <- chat_anthropic_test("Be as terse as possible; no punctuation")
  resp <- chat$chat("What is 1 + 1?", echo = FALSE)
  expect_match(resp, "2")
  expect_equal(chat$last_turn()@tokens > 0, c(TRUE, TRUE))
})

test_that("can make simple streaming request", {
  chat <- chat_anthropic_test("Be as terse as possible; no punctuation")
  resp <- coro::collect(chat$stream("What is 1 + 1?"))
  expect_match(paste0(unlist(resp), collapse = ""), "2")
})

test_that("can list models", {
  test_models(models_anthropic)
})

# Common provider interface -----------------------------------------------

test_that("defaults are reported", {
  expect_snapshot(. <- chat_anthropic())
})

test_that("supports standard parameters", {
  chat_fun <- chat_anthropic_test

  test_params_stop(chat_fun)
})

test_that("all tool variations work", {
  chat_fun <- chat_anthropic_test

  retry_test(test_tools_simple(chat_fun))
  test_tools_async(chat_fun)
  test_tools_parallel(chat_fun)
  # Claude sometimes returns an empty string
  retry_test(test_tools_sequential(chat_fun, total_calls = 6))
})

test_that("can extract data", {
  chat_fun <- chat_anthropic_test

  test_data_extraction(chat_fun)
})

test_that("can use images", {
  chat_fun <- chat_anthropic_test

  test_images_inline(chat_fun)
  test_images_remote(chat_fun)
})

test_that("can use pdfs", {
  chat_fun <- chat_anthropic_test

  test_pdf_local(chat_fun)
})

# Custom features --------------------------------------------------------

test_that("can set beta headers", {
  chat <- chat_anthropic_test(beta_headers = c("a", "b"))
  req <- chat_request(chat$get_provider())
  expect_equal(req$headers$`anthropic-beta`, c("a", "b"))
})

test_that("continues to work after whitespace only outputs (#376)", {
  chat <- chat_anthropic()
  chat$chat("Respond with only two blank lines")
  expect_equal(
    chat$chat("What's 1+1? Just give me the number"),
    ellmer_output("2")
  )
})

test_that("max_tokens is deprecated", {
  expect_snapshot(chat <- chat_anthropic_test(max_tokens = 10))
  expect_equal(chat$get_provider()@params$max_tokens, 10)
})

test_that("strips suffix from model name", {
  provider <- ProviderAnthropic("", model = "", base_url = "", api_key = "")
  expect_equal(
    standardise_model(provider, "claude-3-7-sonnet-latest"),
    "claude-3-7-sonnet"
  )
  expect_equal(
    standardise_model(provider, "claude-3-7-sonnet-20250219"),
    "claude-3-7-sonnet"
  )
})
