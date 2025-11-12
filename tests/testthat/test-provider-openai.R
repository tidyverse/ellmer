# Getting started --------------------------------------------------------

test_that("can make simple request", {
  chat <- chat_openai_test()
  resp <- chat$chat("What is 1 + 1?", echo = FALSE)
  expect_match(resp, "2")
  expect_equal(unname(chat$last_turn()@tokens[1:2] > 0), c(TRUE, TRUE))

  resp <- chat$chat("Double that", echo = FALSE)
  expect_match(resp, "4")
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

# No longer supports stop parameter
# test_that("supports standard parameters", {
#   chat_fun <- chat_openai_test

#   test_params_stop(chat_fun)
# })

test_that("supports tool calling", {
  vcr::local_cassette("openai-v2-tool")
  chat_fun <- chat_openai_test

  test_tools_simple(chat_fun)
})

test_that("can extract data", {
  chat_fun <- chat_openai_test

  test_data_extraction(chat_fun)
})

test_that("can use images", {
  vcr::local_cassette("openai-v2-image")
  # Needs mini to get shape correct
  chat_fun <- \(...) chat_openai_test(model = "gpt-4.1-mini", ...)

  test_images_inline(chat_fun)
  test_images_remote(chat_fun)
})

test_that("can use pdfs", {
  vcr::local_cassette("openai-v2-pdf")
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
  chat$chat("Hi")
  expect_length(chat$last_turn()@json$output[[1]]$content[[1]]$logprobs, 2)
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

test_that("service tier affects pricing", {
  vcr::local_cassette("openai-v2-service-tier")
  chat <- chat_openai_test(service_tier = "priority")
  chat$chat("Tell me a joke")

  last_turn <- chat$last_turn()
  tokens <- as.list(last_turn@tokens)
  priority_cost <- get_token_cost(chat$get_provider(), tokens, "priority")
  expect_equal(last_turn@cost, priority_cost)

  # Confirm we have pricing for the priority tier
  default_cost <- get_token_cost(chat$get_provider(), tokens)
  expect_gt(last_turn@cost, default_cost)
})


test_that("batch retrieve succeeds even if JSON is mangled", {
  local_mocked_bindings(
    openai_download_file = function(provider, id, path) {
      writeLines('{"custom_id": "123", ', path)
    }
  )
  provider <- chat_openai_test()$get_provider()
  out <- batch_retrieve(provider, list(output_file_id = "123"))
  expect_equal(out, list(list(status_code = 500)))
  expect_equal(batch_result_turn(provider, out[[1]]), NULL)
})

test_that("can extract dummy response from malformed JSON", {
  expect_equal(
    openai_json_fallback('{"custom_id": "123", '),
    list(custom_id = "123", response = list(status_code = 500))
  )
})
