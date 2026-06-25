---
name: http-mocking
description: Write tests involving HTTP requests using vcr cassettes. Use when adding or modifying provider tests that make real API calls, recording new cassettes, creating chat_*_test() helper functions for new providers, or setting up vcr cassettes for vignettes and roxygen examples.
---

# HTTP mocking in ellmer

Use this skill when writing or modifying tests that involve LLM provider APIs.

ellmer uses the [vcr](https://docs.ropensci.org/vcr/) package to record and replay HTTP interactions. Tests that call provider APIs can run without credentials by replaying previously recorded responses.

## When to use vcr

Most ellmer tests do not use vcr. Only use vcr when the test makes a real HTTP request to a provider API.

No cassette needed for tests that:
- Construct R objects or parse JSON (e.g. building turns, content types)
- Check request formatting or header construction
- Test internal logic without making HTTP requests

For chat-like behavior without HTTP, use `MockedChat` from `helper-chat.R`:

```r
chat <- mocked_chat(c("response 1", "response 2"))
```

When you don't need a full HTTP round-trip, construct provider and response objects directly:

```r
test_that("value_turn() prices cache writes at 1.25x", {
  provider <- ProviderAnthropic(
    name = "Anthropic",
    base_url = "https://api.anthropic.com/v1",
    model = "claude-sonnet-4-20250514",
    params = list(), extra_args = list(),
    extra_headers = character(), credentials = NULL,
    beta_headers = character(), cache = ""
  )
  result <- list(
    content = list(list(type = "text", text = "ok")),
    stop_reason = "end_turn",
    usage = list(
      input_tokens = 1000, output_tokens = 50,
      cache_creation_input_tokens = 400,
      cache_read_input_tokens = 200
    )
  )

  turn <- value_turn(provider, result)
  expect_equal(unname(turn@tokens), c(1000 + 400, 50, 200))
})
```

In practice, use one cassette to prove a feature works end-to-end with the real API (e.g. "can Anthropic do tool calling"), then test the details like edge cases, parsing logic, and error handling with constructed data.

## Using vcr

### The `vcr::local_cassette()` pattern

Call `vcr::local_cassette()` at the top of `test_that()`, before any code that triggers HTTP requests:

```r
test_that("can make simple request", {
  vcr::local_cassette("anthropic-basic")

  chat <- chat_anthropic_test("Be as terse as possible; no punctuation")
  resp <- chat$chat("What is 1 + 1?")
  expect_match(resp, "2")
})
```

### Provider `_test()` functions

When adding a new provider, create a `chat_{provider}_test()` function that sets `echo = "none"`, `temperature = 0`, and defaults to a low-cost model. See `chat_anthropic_test()` in `R/provider-claude.R` for an example of the pattern.

### Shared test helpers

`tests/testthat/helper-provider.R` defines standardized `test_*()` functions for common provider capabilities (tool calling, image input, data extraction, etc.). Check the file for the current list.

The `helper-provider.R` functions make HTTP requests internally, so they still need a cassette. Pass the `_test()` function (not a chat instance) so the helper can create its own chats:

```r
test_that("supports tool calling", {
  vcr::local_cassette("anthropic-tool")
  chat_fun <- chat_anthropic_test

  test_tools_simple(chat_fun)
})
```


### Credential handling

`key_get()` in `R/utils.R` enables dual-mode operation:

- API key set: runs the real request (and records a cassette if one is active).
- Replaying (`VCR_IS_REPLAYING=TRUE`): returns `""` so the test doesn't error on a missing key.
- Testing without key: skips the test via `testthat::skip()`.

## Managing cassettes

### Cassette naming and storage

Cassettes typically follow a `{provider}-{feature}` naming pattern (e.g., `anthropic-tool.yml`, `openai-pdf.yml`), though some use other descriptive names (e.g., `chat-tools-callbacks.yml`). Choose a name that clearly identifies the provider and feature being tested.

Cassettes are stored in:

- `tests/testthat/_vcr/` for tests
- `vignettes/_vcr/` for vignettes
- `inst/_vcr/` for roxygen examples

### Recording and managing cassettes

To record a cassette:

1. Set the provider's API key as an environment variable.
2. Run the test. If no cassette file exists, vcr records one automatically.
3. Commit the YAML cassette file alongside the test.

To re-record, delete the cassette file and run the test again with a valid API key.

Helper functions in `tests/testthat/helpers-vcr.R`:

- `vcr_clean(url_prefix)` -- deletes all cassettes whose first request URL matches a prefix. Useful when a provider changes its API URL.
- `vcr_rebuild()` -- re-records all cassettes by installing the package, rebuilding vignettes, and running all tests.

## Adding tests for a new provider

1. Create a `chat_{provider}_test()` function (echo = "none", temperature = 0, low-cost model).
2. Write tests using `vcr::local_cassette()` and the shared helpers from `helper-provider.R`.
3. Record cassettes with a valid API key and commit the YAML files in `tests/testthat/_vcr/`.
