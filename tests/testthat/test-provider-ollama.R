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
  test_models(models_ollama)
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
  stub <- ProviderOllama(name = "", base_url = "", model = "")

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
