test_that("value_turn() extracts citations from search_results", {
  provider <- ProviderPerplexity(
    name = "Perplexity",
    base_url = "https://api.perplexity.ai/",
    model = "sonar",
    params = list(),
    extra_args = list(),
    credentials = NULL,
    extra_headers = character()
  )

  result <- list(
    choices = list(list(
      message = list(
        role = "assistant",
        content = "The answer is 42."
      ),
      finish_reason = "stop"
    )),
    search_results = list(
      list(url = "https://example.com", title = "Example Page"),
      list(url = "https://other.com", title = "Other Page")
    ),
    usage = list(prompt_tokens = 10, completion_tokens = 5, total_tokens = 15)
  )

  turn <- value_turn(provider, result)
  expect_length(turn@citations, 2)
  expect_equal(turn@citations[[1]]@url, "https://example.com")
  expect_equal(turn@citations[[1]]@title, "Example Page")
  expect_equal(turn@citations[[2]]@url, "https://other.com")
})
