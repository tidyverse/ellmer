# Getting started --------------------------------------------------------

test_that("can make simple request", {
  chat <- chat_openrouter_test("Be as terse as possible; no punctuation")
  resp <- chat$chat("What is 1 + 1?", echo = FALSE)
  expect_match(resp, "2")
  expect_equal(unname(chat$last_turn()@tokens[1:2] > 0), c(TRUE, TRUE))
})

test_that("can make simple streaming request", {
  chat <- chat_openrouter_test("Be as terse as possible; no punctuation")
  resp <- coro::collect(chat$stream("What is 1 + 1?"))
  expect_match(paste0(unlist(resp), collapse = ""), "2")
})

test_that("handles errors", {
  chat <- chat_openrouter_test(api_args = list(temperature = "hot"))
  expect_snapshot(error = TRUE, {
    chat$chat("What is 1 + 1?", echo = FALSE)
    chat$chat("What is 1 + 1?", echo = TRUE)
  })
})

# Common provider interface -----------------------------------------------

test_that("supports tool calling", {
  chat_fun <- chat_openrouter_test

  test_tools_simple(chat_fun)
})

test_that("can extract data", {
  chat_fun <- chat_openrouter_test

  test_data_extraction(chat_fun)
})

test_that("value_turn() extracts citations from annotations", {
  provider <- ProviderOpenRouter(
    name = "OpenRouter",
    base_url = "https://openrouter.ai/api/v1",
    model = "perplexity/sonar",
    params = list(),
    extra_args = list(),
    credentials = NULL,
    extra_headers = character(),
    preserve_thinking = TRUE
  )

  result <- list(
    choices = list(list(
      message = list(
        role = "assistant",
        content = "The answer is 42.",
        annotations = list(
          list(
            type = "url_citation",
            url_citation = list(
              url = "https://example.com",
              title = "Example Page",
              start_index = 0,
              end_index = 0
            )
          )
        )
      ),
      finish_reason = "stop"
    )),
    usage = list(prompt_tokens = 10, completion_tokens = 5, total_tokens = 15)
  )

  turn <- value_turn(provider, result)
  expect_length(turn@citations, 1)
  expect_equal(turn@citations[[1]]@url, "https://example.com")
  expect_equal(turn@citations[[1]]@title, "Example Page")
})

test_that("can use images", {
  chat_fun <- chat_openrouter_test

  test_images_inline(chat_fun)
  test_images_remote(chat_fun)
})
