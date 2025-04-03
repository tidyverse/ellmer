test_that("chat_azure() is deprecated", {
  lifecycle::expect_deprecated(
    chat_azure("foo", "bar", api_key = "key"),
    "chat_azure_openai"
  )
})

test_that("chat_bedrock() is deprecated", {
  local_mocked_bindings(
    aws_creds_cache = function(...) list(),
    paws_credentials = function(...) list(region = "us-east-1")
  )

  lifecycle::expect_deprecated(
    chat_bedrock(),
    "chat_aws_bedrock"
  )
})

test_that("chat_claude() is deprecated", {
  lifecycle::expect_deprecated(
    chat_claude(api_key = "key"),
    "chat_anthropic"
  )
})

test_that("chat_gemini() is deprecated", {
  lifecycle::expect_deprecated(
    chat_gemini(api_key = "key"),
    "chat_google_gemini"
  )
})
