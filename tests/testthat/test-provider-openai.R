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

test_that("tools can return images", {
  vcr::local_cassette("openai-v2-tool-image")
  chat_fun <- chat_openai_test
  test_tool_image(chat_fun)
})

test_that("can extract data", {
  chat_fun <- chat_openai_test

  test_data_extraction(chat_fun)
})

test_that("can search web pages", {
  vcr::local_cassette("openai-v2-web-search")
  chat_fun <- \(...) chat_openai_test(model = "gpt-4.1", ...)
  test_tool_web_search(
    chat_fun,
    openai_tool_web_search(),
    hint = "The CRAN archive page has this info."
  )
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

  expect_true(has_cost(provider@name, "gpt-4.1"))
  expect_true(has_cost(provider@name, "gpt-4.1-2025-04-14"))
})

# Custom tests -----------------------------------------------------------------

test_that("can retrieve log_probs (#115)", {
  chat <- chat_openai_test(params = params(log_probs = TRUE))
  chat$chat("Hi")
  expect_gt(length(chat$last_turn()@json$output[[1]]$content[[1]]$logprobs), 0)
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
  chat <- chat_openai_test(model = "gpt-4.1-nano", service_tier = "priority")
  chat$chat("Tell me a joke")

  last_turn <- chat$last_turn()
  tokens <- as.list(last_turn@tokens)
  priority_cost <- get_token_cost(
    chat$get_provider()@name,
    chat$get_model_object()@name,
    tokens,
    "priority"
  )
  expect_equal(last_turn@cost, priority_cost)

  # Confirm we have pricing for the priority tier
  default_cost <- get_token_cost(
    chat$get_provider()@name,
    chat$get_model_object()@name,
    tokens
  )
  expect_gt(last_turn@cost, default_cost)
})


test_that("batch retrieve succeeds even if JSON is mangled", {
  local_mocked_bindings(
    openai_download_file = function(provider, id, path) {
      writeLines('{"custom_id": "123", ', path)
    }
  )
  chat <- chat_openai_test()
  provider <- chat$get_provider()
  model <- chat$get_model_object()
  out <- batch_retrieve(provider, list(output_file_id = "123"))
  expect_equal(out, list(list(status_code = 500)))
  expect_equal(batch_result_turn(provider, model, out[[1]]), NULL)
})

test_that("can extract dummy response from malformed JSON", {
  expect_equal(
    openai_json_fallback('{"custom_id": "123", '),
    list(custom_id = "123", response = list(status_code = 500))
  )
})

test_that("value_turn handles NULL service_tier gracefully", {
  provider <- chat_openai_test()$get_provider()

  result <- list(
    output = list(
      list(type = "message", content = list(list(text = "Hello")))
    ),
    usage = NULL,
    service_tier = NULL
  )

  expect_no_error(value_turn(provider, test_model(), result))
})

test_that("value_turn() keeps OpenAI citation metadata without grounded_span", {
  provider <- chat_openai_test()$get_provider()
  annotation <- list(
    type = "url_citation",
    start_index = 4L,
    end_index = 10L,
    url = "https://example.com",
    title = "Example"
  )
  result <- list(
    status = "completed",
    output = list(
      list(
        type = "message",
        content = list(
          list(
            type = "output_text",
            text = "The answer is grounded.",
            annotations = list(annotation)
          )
        )
      )
    ),
    usage = NULL
  )

  contents <- value_turn(provider, test_model(), result)@contents
  expect_s7_class(contents[[1]], ContentText)
  expect_s7_class(contents[[2]], ContentCitation)
  expect_equal(contents[[2]]@source@url, "https://example.com")
  expect_equal(contents[[2]]@source@title, "Example")
  expect_null(contents[[2]]@grounded_span)
  expect_equal(contents[[2]]@extra, annotation)
})

test_that("stream_content() emits OpenAI annotations and web activity", {
  provider <- chat_openai_test()$get_provider()
  annotation <- list(
    type = "url_citation",
    start_index = 0L,
    end_index = 6L,
    url = "https://example.com",
    title = "Example"
  )
  citation <- stream_content(
    provider,
    list(
      type = "response.output_text.annotation.added",
      annotation = annotation
    ),
    completion = NULL
  )
  expect_length(citation, 1)
  expect_s7_class(citation[[1]], ContentCitation)
  expect_null(citation[[1]]@grounded_span)
  expect_equal(citation[[1]]@extra, annotation)

  activity <- stream_content(
    provider,
    list(
      type = "response.output_item.done",
      item = list(
        type = "web_search_call",
        action = list(type = "search", query = "ellmer citations")
      )
    ),
    completion = NULL
  )
  expect_length(activity, 1)
  expect_s7_class(activity[[1]], ContentToolRequestSearch)
  expect_equal(activity[[1]]@query, "ellmer citations")
})

test_that("stream_text() excludes OpenAI annotations", {
  provider <- chat_openai_test()$get_provider()
  annotation <- list(
    type = "url_citation",
    url = "https://example.com",
    title = "Example"
  )

  expect_null(
    stream_text(
      provider,
      list(
        type = "response.output_text.annotation.added",
        annotation = annotation
      )
    )
  )
  expect_equal(
    stream_text(
      provider,
      list(type = "response.output_text.delta", delta = "answer")
    ),
    "answer"
  )
})

test_that("value_turn() handles web_search_call action types", {
  provider <- chat_openai_test()$get_provider()

  make_result <- function(action) {
    list(
      id = "chatcmpl-1",
      object = "chat.completion",
      model = "gpt-4.1-mini",
      choices = list(list(
        index = 0L,
        message = list(
          role = "assistant",
          content = NULL
        ),
        finish_reason = "stop"
      )),
      output = list(
        list(
          id = "ws_1",
          type = "web_search_call",
          status = "completed",
          action = action
        )
      ),
      usage = list(
        prompt_tokens = 10,
        completion_tokens = 5,
        total_tokens = 15
      ),
      service_tier = "default"
    )
  }

  # search action with query
  turn <- value_turn(
    provider,
    test_model(),
    make_result(list(type = "search", query = "test query"))
  )
  expect_equal(turn@contents[[1]]@query, "test query")

  # open_page action with url
  turn <- value_turn(
    provider,
    test_model(),
    make_result(list(type = "open_page", url = "https://example.com"))
  )
  expect_s7_class(turn@contents[[1]], ContentToolRequestFetch)
  expect_equal(turn@contents[[1]]@url, "https://example.com")
  expect_identical(
    as_json(provider, turn@contents[[1]]),
    turn@contents[[1]]@extra
  )

  # find_in_page action with pattern
  turn <- value_turn(
    provider,
    test_model(),
    make_result(list(type = "find_in_page", pattern = "find this"))
  )
  expect_s7_class(turn@contents[[1]], ContentToolRequestSearch)
  expect_equal(turn@contents[[1]]@query, "find this")

  # search action without query
  turn <- value_turn(provider, test_model(), make_result(list(type = "search")))
  expect_equal(turn@contents[[1]]@query, "web search")

  # find_in_page action without pattern falls back
  turn <- value_turn(
    provider,
    test_model(),
    make_result(list(type = "find_in_page"))
  )
  expect_equal(turn@contents[[1]]@query, "web search")
})

# Token counting -----------------------------------------------------------

test_that("can count tokens", {
  vcr::local_cassette("openai-count-tokens")
  test_token_count(chat_openai_test)
})
