# Getting started --------------------------------------------------------

test_that("can make simple request", {
  chat <- chat_openai_compatible_test()
  resp <- chat$chat("What is 1 + 1?", echo = FALSE)
  expect_match(resp, "2")
  expect_equal(unname(chat$last_turn()@tokens[1:2] > 0), c(TRUE, TRUE))
})

test_that("can make simple streaming request", {
  chat <- chat_openai_compatible_test()
  resp <- coro::collect(chat$stream("What is 1 + 1?"))
  expect_match(paste0(unlist(resp), collapse = ""), "2")
})

test_that("can list models", {
  test_models(models_openai)
})


# Common provider interface -----------------------------------------------

test_that("supports standard parameters", {
  chat_fun <- chat_openai_compatible_test

  test_params_stop(chat_fun)
})

test_that("supports tool calling", {
  vcr::local_cassette("openai-tool")
  chat_fun <- chat_openai_compatible_test

  test_tools_simple(chat_fun)
})

test_that("can extract data", {
  chat_fun <- chat_openai_compatible_test

  test_data_extraction(chat_fun)
})

test_that("can use images", {
  vcr::local_cassette("openai-image")
  # Needs mini to get shape correct
  chat_fun <- \(...) chat_openai_compatible_test(model = "gpt-4.1-mini", ...)

  test_images_inline(chat_fun)
  test_images_remote(chat_fun)
})

test_that("can use pdfs", {
  vcr::local_cassette("openai-pdf")
  chat_fun <- chat_openai_compatible_test

  test_pdf_local(chat_fun)
})

test_that("can match prices for some common models", {
  provider <- chat_openai_compatible_test()$get_provider()

  expect_true(has_cost(provider, "gpt-4.1"))
  expect_true(has_cost(provider, "gpt-4.1-2025-04-14"))
})

# Custom tests -----------------------------------------------------------------

test_that("can retrieve log_probs (#115)", {
  chat <- chat_openai_compatible_test(params = params(log_probs = TRUE))
  pieces <- coro::collect(chat$stream("Hi"))

  logprobs <- chat$last_turn()@json$choices[[1]]$logprobs$content
  expect_equal(
    length(logprobs),
    length(pieces) - 2 # leading "" + trailing \n
  )
})

test_that("structured data work with and without wrapper", {
  chat <- chat_openai_compatible_test()
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

test_that("value_turn() treats empty content string as null", {
  stub <- ProviderOpenAICompatible(name = "", base_url = "", model = "")

  result <- list(
    choices = list(list(
      message = list(
        role = "assistant",
        content = "",
        tool_calls = list(list(
          id = "call_1",
          `function` = list(name = "fn", arguments = "{}")
        ))
      )
    ))
  )

  turn <- value_turn(stub, result)
  # Empty content string should not produce ContentText("")
  expect_false(
    any(map_lgl(turn@contents, function(c) S7_inherits(c, ContentText)))
  )
  # Tool request should still be preserved
  expect_equal(length(turn@contents), 1)
  expect_true(S7_inherits(turn@contents[[1]], ContentToolRequest))
})

test_that("empty ContentText is dropped during serialization", {
  stub <- ProviderOpenAICompatible(name = "", base_url = "", model = "")

  # Assistant turn with only an empty ContentText should be dropped entirely
  turn <- AssistantTurn(list(ContentText("")))
  expect_null(as_json(stub, turn))

  # Multiple empty ContentText values are all dropped

  turn <- AssistantTurn(list(ContentText(""), ContentText("")))
  expect_null(as_json(stub, turn))

  # Empty ContentText is stripped but other content is preserved
  turn <- AssistantTurn(list(
    ContentText(""),
    ContentText("Hello")
  ))
  result <- as_json(stub, turn)
  expect_equal(
    result,
    list(list(
      role = "assistant",
      content = list(list(type = "text", text = "Hello"))
    ))
  )
})

test_that("empty ContentText is stripped but tool requests are preserved", {
  stub <- ProviderOpenAICompatible(name = "", base_url = "", model = "")

  turn <- AssistantTurn(list(
    ContentText(""),
    ContentToolRequest(name = "fn", arguments = list(), id = "call_1")
  ))
  result <- as_json(stub, turn)
  expect_equal(result[[1]]$role, "assistant")
  expect_equal(length(result[[1]]$tool_calls), 1)
  expect_null(result[[1]]$content)
})

test_that("stream_content extracts reasoning_content and reasoning", {
  stub <- ProviderOpenAICompatible(name = "", base_url = "", model = "")

  event_content <- list(
    choices = list(list(delta = list(reasoning_content = "think")))
  )
  result <- stream_content(stub, event_content)
  expect_s3_class(result, "ellmer::ContentThinking")
  expect_equal(result@thinking, "think")

  event_reasoning <- list(
    choices = list(list(delta = list(reasoning = "think")))
  )
  result <- stream_content(stub, event_reasoning)
  expect_s3_class(result, "ellmer::ContentThinking")
  expect_equal(result@thinking, "think")

  event_text <- list(choices = list(list(delta = list(content = "hello"))))
  result <- stream_content(stub, event_text)
  expect_s3_class(result, "ellmer::ContentText")
  expect_equal(result@text, "hello")
})

test_that("value_turn extracts reasoning_content and reasoning", {
  stub <- ProviderOpenAICompatible(name = "", base_url = "", model = "")

  result_content <- list(
    choices = list(list(
      message = list(
        role = "assistant",
        reasoning_content = "Let me think...",
        content = "The answer is 42."
      )
    ))
  )
  turn <- value_turn(stub, result_content)
  expect_equal(length(turn@contents), 2)
  expect_s3_class(turn@contents[[1]], "ellmer::ContentThinking")
  expect_equal(turn@contents[[1]]@thinking, "Let me think...")
  expect_s3_class(turn@contents[[2]], "ellmer::ContentText")
  expect_equal(turn@contents[[2]]@text, "The answer is 42.")

  result_reasoning <- list(
    choices = list(list(
      message = list(
        role = "assistant",
        reasoning = "Let me think...",
        content = "The answer is 42."
      )
    ))
  )
  turn <- value_turn(stub, result_reasoning)
  expect_equal(length(turn@contents), 2)
  expect_s3_class(turn@contents[[1]], "ellmer::ContentThinking")
  expect_equal(turn@contents[[1]]@thinking, "Let me think...")
  expect_s3_class(turn@contents[[2]], "ellmer::ContentText")
  expect_equal(turn@contents[[2]]@text, "The answer is 42.")
})

test_that("as_json drops reasoning_content by default", {
  stub <- ProviderOpenAICompatible(name = "", base_url = "", model = "")

  turn <- AssistantTurn(list(
    ContentThinking("Let me think..."),
    ContentText("The answer is 42.")
  ))
  result <- as_json(stub, turn)
  expect_null(result[[1]]$reasoning_content)
  expect_equal(
    result[[1]]$content,
    list(list(type = "text", text = "The answer is 42."))
  )
})

test_that("as_json preserves reasoning_content when preserve_thinking = TRUE", {
  stub <- ProviderOpenAICompatible(
    name = "",
    base_url = "",
    model = "",
    preserve_thinking = TRUE
  )

  turn <- AssistantTurn(list(
    ContentThinking("Let me think..."),
    ContentText("The answer is 42.")
  ))
  result <- as_json(stub, turn)
  expect_equal(result[[1]]$reasoning_content, "Let me think...")
  expect_equal(
    result[[1]]$content,
    list(list(type = "text", text = "The answer is 42."))
  )
})

test_that("as_json specialised for OpenAI", {
  withr::local_options(lifecycle_verbosity = "quiet")
  stub <- ProviderOpenAI(name = "", base_url = "", model = "")

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
