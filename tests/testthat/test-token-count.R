# Unit tests ----------------------------------------------------------------

test_that("default provider errors informatively", {
  provider <- test_provider("Test", "test-model")
  expect_error(
    count_tokens(provider),
    "doesn't support token counting"
  )
})

test_that("Chat$token_count() errors for unsupported providers", {
  chat <- Chat$new(
    provider = test_provider("Test", "test-model"),
    system_prompt = "Hello"
  )
  expect_error(
    chat$token_count("Hi"),
    "doesn't support token counting"
  )
})

# Anthropic ----------------------------------------------------------------

test_that("can count tokens with Anthropic", {
  vcr::local_cassette("anthropic-count-tokens")

  chat <- chat_anthropic_test("You are a scientist")
  result <- chat$token_count("Hello, Claude")
  expect_type(result, "integer")
  expect_gt(result, 0)
})

test_that("can count tokens with Anthropic tools", {
  vcr::local_cassette("anthropic-count-tokens-tools")

  chat <- chat_anthropic_test()
  chat$register_tool(tool(
    function() "2024-01-01",
    name = "current_date",
    description = "Return the current date"
  ))
  result <- chat$token_count("What's the current date?")
  expect_type(result, "integer")
  expect_gt(result, 0)
})

test_that("can count tokens for existing conversation with Anthropic", {
  vcr::local_cassette("anthropic-count-tokens-conversation")

  chat <- chat_anthropic_test("Be terse")
  chat$chat("What is 1+1?")

  result <- chat$token_count("What about 2+2?")
  expect_type(result, "integer")
  expect_gt(result, 0)
})

# Google Gemini ------------------------------------------------------------

test_that("can count tokens with Google Gemini", {
  vcr::local_cassette("google-count-tokens")

  chat <- chat_google_gemini_test("You are a scientist")
  result <- chat$token_count("Hello, Gemini")
  expect_type(result, "integer")

  expect_gt(result, 0)
})

test_that("can count tokens with Google Gemini tools", {
  vcr::local_cassette("google-count-tokens-tools")

  chat <- chat_google_gemini_test()
  chat$register_tool(tool(
    function() "2024-01-01",
    name = "current_date",
    description = "Return the current date"
  ))
  result <- chat$token_count("What's the current date?")
  expect_type(result, "integer")
  expect_gt(result, 0)
})
