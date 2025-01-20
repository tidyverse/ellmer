test_that("can make simple request", {
  chat <- chat_azure(
    system_prompt = "Be as terse as possible; no punctuation",
    endpoint = "https://ai-hwickhamai260967855527.openai.azure.com",
    deployment_id = "gpt-4o-mini"
  )
  resp <- chat$chat("What is 1 + 1?")
  expect_match(resp, "2")
  expect_equal(chat$last_turn()@tokens, c(27, 1))

  resp <- sync(chat$chat_async("What is 1 + 1?"))
  expect_match(resp, "2")
  expect_equal(chat$last_turn()@tokens, c(44, 1))
})

test_that("Azure request headers are generated correctly", {
  turn <- Turn(
    role = "user",
    contents = list(ContentText("What is 1 + 1?"))
  )
  endpoint <- "https://ai-hwickhamai260967855527.openai.azure.com"
  deployment_id <- "gpt-4o-mini"

  # API key.
  p <- ProviderAzure(
    endpoint = endpoint,
    deployment_id = deployment_id,
    api_version = "2024-06-01",
    api_key = "key",
    credentials = default_azure_credentials("key")
  )
  req <- chat_request(p, FALSE, list(turn))
  expect_snapshot(req)

  # Token.
  p <- ProviderAzure(
    endpoint = endpoint,
    deployment_id = deployment_id,
    api_version = "2024-06-01",
    api_key = "",
    credentials = default_azure_credentials("", "token")
  )
  req <- chat_request(p, FALSE, list(turn))
  expect_snapshot(req)

  # Both.
  p <- ProviderAzure(
    endpoint = endpoint,
    deployment_id = deployment_id,
    api_version = "2024-06-01",
    api_key = "key",
    credentials = default_azure_credentials("key", "token")
  )
  req <- chat_request(p, FALSE, list(turn))
  expect_snapshot(req)
})
