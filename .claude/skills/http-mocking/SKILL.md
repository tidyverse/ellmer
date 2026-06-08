---
name: http-mocking
description: Write tests involving HTTP requests using vcr cassettes. Use when adding or modifying provider tests that make real API calls, recording new cassettes, creating chat_*_test() helper functions for new providers, or setting up vcr cassettes for vignettes and roxygen examples.
---

# HTTP mocking with vcr

Use this skill when writing or modifying tests that make HTTP requests to LLM provider APIs.

## Overview

ellmer uses the [vcr](https://docs.ropensci.org/vcr/) package to record and replay HTTP interactions. This means tests that call provider APIs can run without credentials by replaying previously recorded responses.

Only ~10 of 51 test files use vcr. The decision rule is simple: only use vcr when the test actually makes HTTP requests. Tests that construct R objects, parse JSON, or check request formatting do not need vcr.

## Core pattern: `vcr::local_cassette()`

Call `vcr::local_cassette()` at the top of `test_that()`, before any code that triggers HTTP requests:

```r
test_that("can make simple request", {
  vcr::local_cassette("anthropic-basic")

  chat <- chat_anthropic_test("Be as terse as possible; no punctuation")
  resp <- chat$chat("What is 1 + 1?")
  expect_match(resp, "2")
})
```

The cassette auto-cleans up via testthat's local mechanism when the test finishes.

## Cassette naming

Name cassettes `{provider}-{feature}.yml`:

- `anthropic-tool.yml`
- `openai-v2-image.yml`
- `google-gemini-structured-data.yml`

Cassettes are stored in:

- `tests/testthat/_vcr/` for tests
- `vignettes/_vcr/` for vignettes
- `inst/_vcr/` for roxygen examples

## Provider `_test()` functions

Most providers have a `chat_{provider}_test()` function that configures the provider for testing: `echo = "none"`, `temperature = 0`, and a cheap model. Always use the `_test()` variant in tests, never the public constructor.

The canonical example is `chat_anthropic_test()` in `R/provider-claude.R`. The pattern is: set `echo = "none"`, `temperature = 0`, and default to a cheap model. When adding a new provider, create a `_test()` function following this pattern. Check the source for the current default model; pick the cheapest model that supports the features you need.

## Credential handling

`key_get()` in `R/utils.R` enables dual-mode operation:

- API key set: runs the real request (and records a cassette if one is active).
- Replaying (`VCR_IS_REPLAYING=TRUE`): returns `""` so the test doesn't error on a missing key.
- Testing without key: skips the test via `testthat::skip()`.

This means contributors without API keys can still run the full test suite against recorded cassettes.

## Shared test helpers

`tests/testthat/helper-provider.R` defines reusable test functions that standardize how provider capabilities are tested:

- `test_tools_simple(chat_fun)` -- tool calling
- `test_tool_image(chat_fun)` -- tools returning images
- `test_tool_web_fetch(chat_fun, tool)` -- web fetch
- `test_tool_web_search(chat_fun, tool)` -- web search
- `test_data_extraction(chat_fun)` -- structured data extraction
- `test_images_inline(chat_fun)` -- inline image input
- `test_images_remote(chat_fun)` -- remote image input
- `test_pdf_local(chat_fun)` -- PDF input
- `test_params_stop(chat_fun)` -- stop sequences
- `test_models(models_fun)` -- model listing

Use these instead of writing custom logic. Each shared helper needs its own cassette in the calling test:

```r
test_that("supports tool calling", {
  vcr::local_cassette("anthropic-tool")
  chat_fun <- chat_anthropic_test

  test_tools_simple(chat_fun)
})
```

## Keeping mocks minimal

This is the most important principle. Cassettes add maintenance burden, so minimize them:

1. Only use vcr for real HTTP calls. Tests that construct R objects, parse responses, or check request formatting don't need cassettes. See the `value_turn()` tests in `test-provider-claude.R` for examples of tests that build result objects directly.

2. Reuse shared test helpers. One `test_tools_simple()` call covers tool calling for a provider. Don't write custom tool tests that need their own cassettes.

3. Test edge cases with constructed data. `MockedChat` from `helper-chat.R` lets you test chat-dependent behavior without HTTP:

   ```r
   chat <- mocked_chat(c("response 1", "response 2"))
   ```

4. Don't record cassettes for tests that can be done without HTTP. For example, testing header construction, JSON serialization, or request building doesn't need vcr.

## Recording and managing cassettes

To record a cassette:

1. Set the provider's API key as an environment variable.
2. Run the test. If no cassette file exists, vcr records one automatically.
3. Commit the YAML cassette file alongside the test.

To re-record, delete the cassette file and run the test again with a valid API key.

Helper functions in `tests/testthat/helpers-vcr.R`:

- `vcr_clean(url_prefix)` -- deletes all cassettes whose first request URL matches a prefix. Useful when a provider changes its API URL.
- `vcr_rebuild()` -- re-records all cassettes by installing the package, rebuilding vignettes, and running all tests.

## Vignettes and examples

Vignettes use `eval_vignette()` from `R/utils.R` to decide whether to evaluate code chunks. It returns `TRUE` if API keys are available or pre-recorded cassettes exist.

Roxygen examples use a start/end pattern:

```r
#' @examples
#' vcr_example_start("example-name")
#' chat <- chat_openai()
#' chat$chat("Hello")
#' vcr_example_end()
```

These record to `inst/_vcr/`.

## Checklist: adding tests for a new provider

When adding vcr tests for a new provider:

- [ ] Create a `chat_{provider}_test()` function (echo = "none", temperature = 0, cheap model).
- [ ] Write tests using `vcr::local_cassette("{provider}-{feature}")`.
- [ ] Reuse shared helpers from `helper-provider.R` where possible.
- [ ] Only add cassettes for tests that make real HTTP requests.
- [ ] Name cassettes `{provider}-{feature}.yml`.
- [ ] Record cassettes with a valid API key.
- [ ] Commit the YAML cassette files in `tests/testthat/_vcr/`.
