# Getting started --------------------------------------------------------

test_that("can make simple request", {
  chat <- chat_lmstudio_test("Be as terse as possible; no punctuation")
  resp <- chat$chat("What is 1 + 1?", echo = FALSE)
  expect_match(resp, "2")
  expect_equal(unname(chat$last_turn()@tokens[1:2] > 0), c(TRUE, TRUE))
})

test_that("can make simple streaming request", {
  chat <- chat_lmstudio_test("Be as terse as possible; no punctuation")
  resp <- coro::collect(chat$stream("What is 1 + 1?"))
  expect_match(paste0(unlist(resp), collapse = ""), "2")
})

test_that("can list models", {
  skip_if_no_lmstudio()
  test_models(models_lmstudio)
})

test_that("model listing joins the context window from the native endpoint", {
  local_mocked_responses(function(req) {
    if (grepl("api/v1/models", req$url)) {
      response_json(
        body = list(
          models = list(
            list(key = "qwen/qwen3-0.6b", max_context_length = 32768),
            list(key = "nomic-embed-text-v1.5", max_context_length = 2048)
          )
        )
      )
    } else {
      response_json(
        body = list(
          data = list(
            list(id = "qwen/qwen3-0.6b"),
            list(id = "nomic-embed-text-v1.5")
          )
        )
      )
    }
  })

  models <- models_lmstudio(credentials = \() "")
  expect_equal(models$context_window, c(32768L, 2048L))
})

test_that("model listing survives the native endpoint changing shape", {
  local_mocked_responses(function(req) {
    if (grepl("api/v1/models", req$url)) {
      response_json(body = list(models = list(list(max_context_length = 4096))))
    } else {
      response_json(body = list(data = list(list(id = "qwen/qwen3-0.6b"))))
    }
  })

  expect_equal(
    models_lmstudio(credentials = \() "")$context_window,
    NA_integer_
  )
})

test_that("model listing survives a server without the native endpoint", {
  local_mocked_responses(function(req) {
    if (grepl("api/v1/models", req$url)) {
      # LM Studio answers an unknown route with 200 and an error body
      response_json(body = list(error = "Unexpected endpoint or method."))
    } else {
      response_json(body = list(data = list(list(id = "qwen/qwen3-0.6b"))))
    }
  })

  expect_equal(
    models_lmstudio(credentials = \() "")$context_window,
    NA_integer_
  )
})

test_that("includes list of models in error message if `model` is missing", {
  skip_if_no_lmstudio()

  local_mocked_bindings(
    models_lmstudio = function(...) {
      list(id = c("llama3", "google/gemma-4-26b-a4b"))
    }
  )

  expect_snapshot(chat_lmstudio(), error = TRUE)
})

test_that("checks that requested model is loaded", {
  skip_if_no_lmstudio()
  local_mocked_bindings(
    models_lmstudio = function(...) list(id = "llama3")
  )
  expect_snapshot(
    chat_lmstudio(model = "not-a-real-model"),
    error = TRUE
  )
})

# Common provider interface -----------------------------------------------

test_that("supports tool calling", {
  chat_fun <- chat_lmstudio_test
  test_tools_simple(chat_fun)
})
