# Getting started --------------------------------------------------------

test_that("can make simple request", {
  chat <- chat_openai_test()
  resp <- chat$chat("What is 1 + 1?", echo = FALSE)
  expect_match(resp, "2")
  expect_equal(chat$last_turn()@tokens[1:2] > 0, c(TRUE, TRUE))
})

test_that("can make simple streaming request", {
  chat <- chat_openai_test()
  resp <- coro::collect(chat$stream("What is 1 + 1?"))
  expect_match(paste0(unlist(resp), collapse = ""), "2")
})

test_that("can list models", {
  test_models(models_openai)
})


# Common provider interface -----------------------------------------------

test_that("defaults are reported", {
  expect_snapshot(. <- chat_openai())
})

test_that("supports standard parameters", {
  chat_fun <- chat_openai_test

  test_params_stop(chat_fun)
})

test_that("supports tool calling", {
  vcr::local_cassette("openai-tool")
  chat_fun <- chat_openai_test

  test_tools_simple(chat_fun)
})

test_that("can extract data", {
  chat_fun <- chat_openai_test

  test_data_extraction(chat_fun)
})

test_that("can use images", {
  vcr::local_cassette("openai-image")
  # Needs mini to get shape correct
  chat_fun <- \(...) chat_openai_test(model = "gpt-4.1-mini", ...)

  test_images_inline(chat_fun)
  test_images_remote(chat_fun)
})

test_that("can use pdfs", {
  vcr::local_cassette("openai-pdf")
  chat_fun <- chat_openai_test

  test_pdf_local(chat_fun)
})

test_that("can match prices for some common models", {
  provider <- chat_openai_test()$get_provider()

  expect_true(has_cost(provider, "gpt-4.1"))
  expect_true(has_cost(provider, "gpt-4.1-2025-04-14"))
})

# Custom tests -----------------------------------------------------------------

test_that("can retrieve log_probs (#115)", {
  chat <- chat_openai_test(params = params(log_probs = TRUE))
  pieces <- coro::collect(chat$stream("Hi"))

  logprobs <- chat$last_turn()@json$choices[[1]]$logprobs$content
  expect_equal(
    length(logprobs),
    length(pieces) - 2 # leading "" + trailing \n
  )
})

test_that("structured data work with and without wrapper", {
  chat <- chat_openai_test()
  out <- chat$chat_structured(
    "Extract the number: apple, green, eleven",
    type = type_number()
  )
  expect_equal(out, 11)

  out <- chat$chat_structured(
    "Extract the number: apple, green, eleven",
    type = type_object(number = type_number())
  )
  expect_equal(out, list(number = 11))
})

# Custom -----------------------------------------------------------------

test_that("as_json specialised for OpenAI", {
  stub <- ProviderOpenAI(name = "", base_url = "", api_key = "", model = "")

  expect_snapshot(
    as_json(stub, type_object(.additional_properties = TRUE)),
    error = TRUE
  )

  obj <- type_object(x = type_number(required = FALSE))
  expect_equal(
    as_json(stub, obj),
    list(
      type = "object",
      description = "",
      properties = list(x = list(type = c("number", "null"), description = "")),
      required = list("x"),
      additionalProperties = FALSE
    )
  )
})

test_that("seed is deprecated, but still honored", {
  expect_snapshot(chat <- chat_openai_test(seed = 1))
  expect_equal(chat$get_provider()@params$seed, 1)

  # NULL is also ignored since that's what subclasses use
  expect_no_warning(chat_openai_test(seed = NULL))
})
