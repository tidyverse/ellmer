test_that("can make simple batch request", {
  vcr::local_cassette("anthropic-batch")

  chat <- chat_anthropic_test("Be as terse as possible; no punctuation")
  resp <- chat$chat("What is 1 + 1?")
  expect_match(resp, "2")
  expect_equal(chat$last_turn()@tokens[1:2] > 0, c(TRUE, TRUE))
})

test_that("can make simple streaming request", {
  chat <- chat_anthropic_test("Be as terse as possible; no punctuation")
  resp <- coro::collect(chat$stream("What is 1 + 1?"))
  expect_match(paste0(unlist(resp), collapse = ""), "2")
})

test_that("can list models", {
  vcr::local_cassette("anthropic-list-models")

  test_models(models_anthropic)
})

# Common provider interface -----------------------------------------------

test_that("defaults are reported", {
  expect_snapshot(. <- chat_anthropic())
})

test_that("supports standard parameters", {
  vcr::local_cassette("anthropic-standard-params")
  chat_fun <- chat_anthropic_test

  test_params_stop(chat_fun)
})

test_that("supports tool calling", {
  vcr::local_cassette("anthropic-tool")
  chat_fun <- chat_anthropic_test

  test_tools_simple(chat_fun)
})

test_that("can extract data", {
  vcr::local_cassette("anthropic-structured-data")
  chat_fun <- chat_anthropic_test

  test_data_extraction(chat_fun)
})

test_that("can use images", {
  vcr::local_cassette("anthropic-images")
  chat_fun <- chat_anthropic_test

  test_images_inline(chat_fun)
  test_images_remote(chat_fun)
})

test_that("can use pdfs", {
  vcr::local_cassette("anthropic-pdfs")
  chat_fun <- chat_anthropic_test

  test_pdf_local(chat_fun)
})

# Custom features --------------------------------------------------------

test_that("can set beta headers", {
  chat <- chat_anthropic_test(beta_headers = c("a", "b"))
  req <- chat_request(chat$get_provider())
  headers <- req_get_headers(req)
  expect_equal(headers$`anthropic-beta`, "a,b")
})

test_that("continues to work after whitespace only outputs (#376)", {
  vcr::local_cassette("anthropic-whitespace")

  chat <- chat_anthropic_test()
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

test_that("can match prices for some common models", {
  provider <- chat_anthropic_test()$get_provider()
  
  expect_true(has_cost(provider, "claude-sonnet-4-20250514"))
  expect_true(has_cost(provider, "claude-3-7-sonnet-latest"))
})
