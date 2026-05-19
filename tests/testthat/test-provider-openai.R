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

test_that("value_turn handles NULL service_tier gracefully", {
  provider <- chat_openai_test()$get_provider()

  result <- list(
    output = list(
      list(type = "message", content = list(list(text = "Hello")))
    ),
    usage = NULL,
    service_tier = NULL
  )

  expect_no_error(value_turn(provider, result))
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
    make_result(list(type = "search", query = "test query"))
  )
  expect_equal(turn@contents[[1]]@query, "test query")

  # open_page action with url
  turn <- value_turn(
    provider,
    make_result(list(type = "open_page", url = "https://example.com"))
  )
  expect_equal(turn@contents[[1]]@query, "https://example.com")

  # find_in_page action with queries
  turn <- value_turn(
    provider,
    make_result(list(type = "find_in_page", queries = list("find this")))
  )
  expect_equal(turn@contents[[1]]@query, "find this")

  # search action without query
  turn <- value_turn(provider, make_result(list(type = "search")))
  expect_equal(turn@contents[[1]]@query, "web search")

  # find_in_page action with empty queries falls back
  turn <- value_turn(
    provider,
    make_result(list(type = "find_in_page", queries = list()))
  )
  expect_equal(turn@contents[[1]]@query, "web search")
})

# MCP tool handling -----------------------------------------------------------

test_that("value_turn() parses mcp_call output", {
  provider <- chat_openai_test()$get_provider()

  result <- list(
    output = list(
      list(
        type = "mcp_call",
        id = "mcp_call_1",
        name = "read_wiki_structure",
        server_label = "deepwiki",
        arguments = '{"repo_name":"tidyverse/ellmer"}',
        output = "# Page: Overview\nThe ellmer R package...",
        error = NULL
      )
    ),
    usage = list(input_tokens = 10, output_tokens = 5),
    service_tier = "default"
  )

  turn <- value_turn(provider, result)
  expect_length(turn@contents, 2)

  req <- turn@contents[[1]]
  expect_s7_class(req, ContentMcpToolRequest)
  expect_equal(req@id, "mcp_call_1")
  expect_equal(req@name, "read_wiki_structure")
  expect_equal(req@server_name, "deepwiki")
  expect_equal(req@json$input, list(repo_name = "tidyverse/ellmer"))

  resp <- turn@contents[[2]]
  expect_s7_class(resp, ContentMcpToolResult)
  expect_equal(resp@tool_use_id, "mcp_call_1")
  expect_equal(resp@is_error, FALSE)
  expect_equal(
    resp@content,
    list(list(
      type = "text",
      text = "# Page: Overview\nThe ellmer R package..."
    ))
  )
})

test_that("value_turn() parses mcp_call with error", {
  provider <- chat_openai_test()$get_provider()

  result <- list(
    output = list(
      list(
        type = "mcp_call",
        id = "mcp_call_2",
        name = "execute_query",
        server_label = "db_server",
        arguments = '{"query":"SELECT 1"}',
        output = NULL,
        error = "Connection refused"
      )
    ),
    usage = list(input_tokens = 10, output_tokens = 5),
    service_tier = "default"
  )

  turn <- value_turn(provider, result)
  resp <- turn@contents[[2]]
  expect_s7_class(resp, ContentMcpToolResult)
  expect_equal(resp@is_error, TRUE)
  expect_equal(
    resp@content,
    list(list(type = "text", text = "Connection refused"))
  )
})

test_that("value_turn() parses mcp_list_tools output", {
  provider <- chat_openai_test()$get_provider()

  result <- list(
    output = list(
      list(
        type = "mcp_list_tools",
        server_label = "deepwiki",
        tools = list(
          list(
            name = "read_wiki_structure",
            description = "Get docs structure",
            input_schema = list(type = "object")
          ),
          list(
            name = "read_wiki_contents",
            description = "Get docs contents",
            input_schema = list(type = "object")
          )
        )
      ),
      list(
        type = "message",
        content = list(list(text = "I found some tools."))
      )
    ),
    usage = list(input_tokens = 10, output_tokens = 5),
    service_tier = "default"
  )

  turn <- value_turn(provider, result)
  expect_length(turn@contents, 2)

  mcp_list <- turn@contents[[1]]
  expect_s7_class(mcp_list, ContentMcpListTools)
  expect_equal(mcp_list@server_name, "deepwiki")
  expect_length(mcp_list@tools, 2)
  expect_equal(mcp_list@tools[[1]]$name, "read_wiki_structure")

  expect_s7_class(turn@contents[[2]], ContentText)
})

test_that("value_turn() errors on mcp_approval_request", {
  provider <- chat_openai_test()$get_provider()

  result <- list(
    output = list(
      list(
        type = "mcp_approval_request",
        id = "mcpar_1",
        name = "execute_query",
        server_label = "db_server",
        arguments = '{"query":"DROP TABLE users"}'
      )
    ),
    usage = list(input_tokens = 10, output_tokens = 5),
    service_tier = "default"
  )

  expect_error(
    value_turn(provider, result),
    "require_approval"
  )
})

test_that("as_json round-trips mcp_call without duplication", {
  provider <- chat_openai_test()$get_provider()

  mcp_output <- list(
    type = "mcp_call",
    id = "mcp_call_1",
    name = "read_wiki_structure",
    server_label = "deepwiki",
    arguments = '{"repo_name":"tidyverse/ellmer"}',
    output = "Result text",
    error = NULL
  )
  result <- list(
    output = list(mcp_output),
    usage = list(input_tokens = 10, output_tokens = 5),
    service_tier = "default"
  )

  turn <- value_turn(provider, result)
  json_items <- as_json(provider, turn@contents, role = "assistant")
  non_null <- compact(json_items)
  expect_length(non_null, 1)
  expect_equal(non_null[[1]]$type, "mcp_call")
})
