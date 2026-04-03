test_that("can make simple batch request", {
  chat <- chat_aws_bedrock_test("Be as terse as possible; no punctuation")
  resp <- chat$chat("What is 1 + 1?", echo = FALSE)
  expect_match(resp, "2")
  expect_equal(unname(chat$last_turn()@tokens[1:2] > 0), c(TRUE, TRUE))
})

test_that("can make simple streaming request", {
  chat <- chat_aws_bedrock_test("Be as terse as possible; no punctuation")
  resp <- coro::collect(chat$stream("What is 1 + 1?"))
  expect_match(paste0(unlist(resp), collapse = ""), "2")
})

test_that("can list models", {
  test_models(models_aws_bedrock)
})

test_that("can set api args", {
  chat <- chat_aws_bedrock_test(
    api_args = list(inferenceConfig = list(maxTokens = 1))
  )
  result <- chat$chat("Who are the reindeer?")
  expect_true(nchar(result) < 10)
})

test_that("api args overwrite params", {
  chat <- chat_aws_bedrock_test(
    api_args = list(inferenceConfig = list(maxTokens = 1)),
    params = params(max_tokens = 100)
  )
  result <- chat$chat("Who are the reindeer?")
  expect_true(nchar(result) < 10)
})

test_that("handles errors", {
  chat <- chat_aws_bedrock_test(
    api_args = list(inferenceConfig = list(temperature = "hot"))
  )
  expect_snapshot(error = TRUE, {
    chat$chat("What is 1 + 1?", echo = FALSE)
    chat$chat("What is 1 + 1?", echo = TRUE)
  })
})

# Common provider interface -----------------------------------------------

test_that("defaults are reported", {
  expect_snapshot(. <- chat_aws_bedrock())
})

test_that("supports tool calling", {
  chat_fun <- chat_aws_bedrock_test

  test_tools_simple(chat_fun)
})

test_that("can extract data", {
  chat_fun <- chat_aws_bedrock_test

  test_data_extraction(chat_fun)
})

test_that("can use images", {
  chat_fun <- chat_aws_bedrock_test

  test_images_inline(chat_fun)
  test_images_remote_error(chat_fun)
})

test_that("can use pdfs", {
  chat_fun <- chat_aws_bedrock_test

  test_pdf_local(chat_fun)
})

# Prompt caching ----------------------------------------------------------

has_cache_point <- function(content) {
  any(vapply(content, function(b) "cachePoint" %in% names(b), logical(1)))
}

block_types <- function(content) {
  vapply(content, function(b) names(b)[[1]], character(1))
}

test_that("as_bedrock_cache_point() resolves 'auto' for known models", {
  # Anthropic models (direct and cross-region)
  expect_equal(
    as_bedrock_cache_point("auto", "anthropic.claude-3-5-haiku-20241022-v1:0"),
    "5m"
  )
  expect_equal(
    as_bedrock_cache_point(
      "auto",
      "us.anthropic.claude-sonnet-4-5-20250929-v1:0"
    ),
    "5m"
  )

  # Amazon Nova models (direct and cross-region)
  expect_equal(as_bedrock_cache_point("auto", "amazon.nova-pro-v1:0"), "5m")
  expect_equal(as_bedrock_cache_point("auto", "us.amazon.nova-lite-v1:0"), "5m")

  # Unsupported models
  expect_equal(as_bedrock_cache_point("auto", "zai.glm-5"), "none")
  expect_equal(
    as_bedrock_cache_point("auto", "meta.llama3-1-8b-instruct-v1:0"),
    "none"
  )
})

test_that("as_bedrock_cache_point() passes through non-auto values", {
  expect_equal(as_bedrock_cache_point("5m", "zai.glm-5"), "5m")
  expect_equal(as_bedrock_cache_point("1h", "zai.glm-5"), "1h")
  expect_equal(
    as_bedrock_cache_point("none", "anthropic.claude-3-5-haiku-20241022-v1:0"),
    "none"
  )
})

test_that("cache points are inserted in last turn when cache is enabled", {
  provider <- test_aws_bedrock_provider(cache_point = "5m")

  # Non-last turn should not have a cache point
  result <- as_json(provider, UserTurn("Hello"), is_last = FALSE)
  expect_disjoint(block_types(result$content), "cachePoint")

  # Last turn should have a cache point appended
  result <- as_json(provider, UserTurn("Hello"), is_last = TRUE)
  last_block <- result$content[[length(result$content)]]
  expect_equal(last_block, list(cachePoint = list(type = "default")))
})

test_that("cache points are omitted when cache = 'none'", {
  provider <- test_aws_bedrock_provider(cache_point = "none")

  result <- as_json(provider, UserTurn("Hello"), is_last = TRUE)
  expect_disjoint(block_types(result$content), "cachePoint")
})

test_that("cache TTL is included for '1h' but not '5m'", {
  provider_5m <- test_aws_bedrock_provider(cache_point = "5m")
  provider_1h <- test_aws_bedrock_provider(cache_point = "1h")

  # 5m: cachePoint should be list(type = "default") with no ttl
  cp_5m <- bedrock_cache_point(provider_5m)
  expect_equal(cp_5m, list(list(cachePoint = list(type = "default"))))

  # 1h: cachePoint should include ttl = "1h"
  cp_1h <- bedrock_cache_point(provider_1h)
  expect_equal(
    cp_1h,
    list(list(cachePoint = list(type = "default", ttl = "1h")))
  )
})

test_that("cache point is only on the last turn in multi-turn conversations", {
  provider <- test_aws_bedrock_provider(cache_point = "5m")

  # Intermediate turns (is_last = FALSE) should not have cache points
  r1 <- as_json(provider, UserTurn("Hello"), is_last = FALSE)
  expect_false(has_cache_point(r1$content))

  r2 <- as_json(
    provider,
    AssistantTurn(list(ContentText("Hi there!"))),
    is_last = FALSE
  )
  expect_false(has_cache_point(r2$content))

  # Last turn should have a cache point
  r3 <- as_json(provider, UserTurn("How are you?"), is_last = TRUE)
  expect_true(has_cache_point(r3$content))
})

test_that("bedrock_cache_point() is added to the system prompt", {
  provider <- test_aws_bedrock_provider(cache_point = "5m")
  cp <- bedrock_cache_point(provider)

  # Mirrors the system prompt construction in chat_request()
  system <- c(
    list(list(text = "You are a helpful assistant.")),
    cp
  )

  expect_length(system, 2)
  expect_equal(system[[1]], list(text = "You are a helpful assistant."))
  expect_equal(system[[2]], list(cachePoint = list(type = "default")))

  # cache = "none" should not add a cache point
  provider_none <- test_aws_bedrock_provider(cache_point = "none")
  expect_equal(bedrock_cache_point(provider_none), list())
})

# Provider idiosynchronies -----------------------------------------------

test_that("continues to work after whitespace only outputs (#376)", {
  chat <- chat_aws_bedrock_test()
  chat$chat("Respond with only two blank lines")
  expect_equal(
    chat$chat("What's 1+1? Just give me the number"),
    ellmer_output("2")
  )
})

# Auth --------------------------------------------------------------------

test_that("AWS credential caching works as expected", {
  # Mock AWS credentials for different profiles.
  local_mocked_bindings(
    locate_aws_credentials = function(profile) {
      if (!is.null(profile) && profile == "test") {
        list(
          access_key_id = "key1",
          secret_key = "secret1",
          expiration = Sys.time() + 3600
        )
      } else {
        list(
          access_key_id = "key2",
          secret_key = "secret2",
          expiration = Sys.time() + 3600
        )
      }
    }
  )

  creds1 <- paws_credentials(profile = "test", reauth = TRUE)
  creds2 <- paws_credentials(profile = NULL, reauth = TRUE)

  # Verify different credentials were returned.
  expect_false(identical(creds1, creds2))
  expect_equal(creds1$access_key_id, "key1")
  expect_equal(creds2$access_key_id, "key2")

  # Verify cached credentials match original ones.
  expect_identical(creds1, paws_credentials(profile = "test"))
  expect_identical(creds2, paws_credentials(profile = NULL))

  # Simulate a cache entry that has expired.
  creds_modified <- creds1
  creds_modified$expiration <- Sys.time() - 5
  aws_creds_cache(profile = "test")$set(creds_modified)

  # Ensure the new credentials have been updated.
  expect_false(identical(creds_modified, paws_credentials(profile = "test")))
  expect_false(identical(creds1, paws_credentials(profile = "test")))
  expect_false(identical(creds2, paws_credentials(profile = "test")))
})
