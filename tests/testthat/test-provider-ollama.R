# Getting started --------------------------------------------------------

test_that("can make simple request", {
  chat <- chat_ollama_test("Be as terse as possible; no punctuation")
  resp <- chat$chat("What is 1 + 1?", echo = FALSE)
  expect_match(resp, "2")
  expect_equal(unname(chat$last_turn()@tokens[1:2] > 0), c(TRUE, TRUE))
})

test_that("can make simple streaming request", {
  chat <- chat_ollama_test("Be as terse as possible; no punctuation")
  resp <- coro::collect(chat$stream("What is 1 + 1?"))
  expect_match(paste0(unlist(resp), collapse = ""), "2")
})

test_that("can list models", {
  skip_if_no_ollama()
  test_models(models_ollama, has_context_window = TRUE)
})

test_that("model listing reports capabilities and the context window", {
  local_ollama_cache()
  local_mocked_responses(function(req) {
    if (grepl("api/tags", req$url)) {
      response_json(
        body = list(
          models = list(
            list(
              name = "gpt-oss:20b",
              modified_at = "2026-01-01T00:00:00Z",
              size = 1
            )
          )
        )
      )
    } else {
      response_json(
        body = list(
          capabilities = list("completion", "tools"),
          model_info = list(
            "general.architecture" = "gptoss",
            "gptoss.context_length" = 131072
          )
        )
      )
    }
  })

  models <- models_ollama(credentials = \() "")
  expect_equal(models$id, "gpt-oss:20b")
  expect_equal(models$capabilities, "completion,tools")
  expect_equal(models$context_window, 131072L)
})

test_that("context length is read from the architecture-specific field", {
  details <- list(
    model_info = list(
      "general.architecture" = "gemma3",
      "gemma3.context_length" = 8192
    )
  )
  expect_equal(ollama_context_length(details), 8192L)
  expect_equal(ollama_context_length(list(model_info = list())), NA_integer_)
  expect_equal(ollama_context_length(NULL), NA_integer_)
})

test_that("includes list of models in error message if `model` is missing", {
  skip_if_no_ollama()

  local_mocked_bindings(
    models_ollama = function(...) list(id = "llama3")
  )

  expect_snapshot(chat_ollama(), error = TRUE)
})

test_that("checks that requested model is installed", {
  skip_if_no_ollama()
  local_mocked_bindings(
    models_ollama = function(...) list(id = "llama3")
  )
  expect_snapshot(
    chat_ollama(model = "not-a-real-model"),
    error = TRUE
  )
})

# Common provider interface -----------------------------------------------

test_that("supports tool calling", {
  chat_fun <- chat_ollama_test
  test_tools_simple(chat_fun)

  # Work, but don't match quite the right format because they include
  # additional (blank) ContentText
})

# Currently no other tests because I can't find a model that returns reliable
# results and is reasonably performant.

# Custom -----------------------------------------------------------------

test_that("as_json specialised for Ollama", {
  withr::local_options(lifecycle_verbosity = "quiet")
  stub <- ProviderOllama(name = "", base_url = "")

  expect_snapshot(
    as_json(stub, type_object(.additional_properties = TRUE)),
    error = TRUE
  )

  obj <- type_object(
    x = type_number(required = FALSE),
    y = type_string(required = TRUE)
  )
  expect_equal(
    as_json(stub, obj),
    list(
      type = "object",
      description = "",
      properties = list(
        x = list(type = c("number"), description = ""),
        y = list(type = c("string"), description = "")
      ),
      required = list("y"),
      additionalProperties = FALSE
    )
  )
})
