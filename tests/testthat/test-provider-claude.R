test_anthropic_provider <- function() {
  ProviderAnthropic(
    name = "Anthropic",
    base_url = "https://api.anthropic.com/v1",
    model = "claude-sonnet-4-20250514",
    params = list(),
    extra_args = list(),
    extra_headers = character(),
    credentials = NULL,
    beta_headers = character(),
    cache = ""
  )
}

test_that("can make simple request", {
  vcr::local_cassette("anthropic-basic")

  chat <- chat_anthropic_test("Be as terse as possible; no punctuation")
  resp <- chat$chat("What is 1 + 1?")
  expect_match(resp, "2")
  expect_equal(unname(chat$last_turn()@tokens[1:2] > 0), c(TRUE, TRUE))
})

test_that("can make simple streaming request", {
  chat <- chat_anthropic_test(
    "Be as terse as possible; no punctuation"
  )
  resp <- coro::collect(chat$stream("What is 1 + 1?"))
  expect_match(paste0(unlist(resp), collapse = ""), "2")
})

test_that("can list models", {
  vcr::local_cassette("anthropic-list-models")

  test_models(models_anthropic)
})

# Common provider interface -----------------------------------------------

test_that("defaults are reported", {
  expect_snapshot(. <- chat_anthropic())
})

test_that("supports standard parameters", {
  vcr::local_cassette("anthropic-standard-params")
  test_params_stop(chat_anthropic_test)
})

test_that("supports tool calling", {
  vcr::local_cassette("anthropic-tool")
  chat_fun <- chat_anthropic_test

  test_tools_simple(chat_fun)
})

test_that("can fetch web pages", {
  vcr::local_cassette("anthropic-web-fetch")
  chat_fun <- \(...) {
    chat_anthropic_test(..., beta_headers = "web-fetch-2025-09-10")
  }
  test_tool_web_fetch(chat_fun, claude_tool_web_fetch())
})

test_that("can search web pages", {
  vcr::local_cassette("anthropic-web-search")
  chat_fun <- \(...) chat_anthropic_test(...)
  test_tool_web_search(chat_fun, claude_tool_web_search())
})

test_that("can use MCP connector tools", {
  vcr::local_cassette("anthropic-mcp-tool")

  chat <- chat_anthropic_test(
    system_prompt = "Use the read_wiki_structure tool to answer the question. Be brief.",
    beta_headers = "mcp-client-2025-11-20",
    api_args = list(
      mcp_servers = list(
        list(
          type = "url",
          url = "https://mcp.deepwiki.com/mcp",
          name = "deepwiki"
        )
      ),
      tools = list(
        list(type = "mcp_toolset", mcp_server_name = "deepwiki")
      )
    )
  )

  result <- chat$chat(
    "What are the top-level sections in the tidyverse/ellmer documentation?"
  )
  expect_match(result, "Core Architecture")

  turns <- chat$get_turns()
  assistant_turn <- turns[[length(turns)]]
  contents <- assistant_turn@contents

  mcp_requests <- keep(contents, S7_inherits, ContentMcpToolRequest)
  mcp_results <- keep(contents, S7_inherits, ContentMcpToolResult)

  expect_length(mcp_requests, 1)
  expect_length(mcp_results, 1)
  expect_equal(mcp_requests[[1]]@server_name, "deepwiki")
  expect_equal(mcp_requests[[1]]@name, "read_wiki_structure")
  expect_null(mcp_results[[1]]@error)
  expect_true(length(mcp_results[[1]]@content) > 0)
})

test_that("tools can return images", {
  vcr::local_cassette("anthropic-tool-image")
  chat_fun <- chat_anthropic_test
  test_tool_image(chat_fun)
})

test_that("can extract data", {
  vcr::local_cassette("anthropic-structured-data")
  chat_fun <- chat_anthropic_test

  test_data_extraction(chat_fun)
})

test_that("can use images", {
  vcr::local_cassette("anthropic-images")
  chat_fun <- chat_anthropic_test

  test_images_inline(chat_fun)
  test_images_remote(chat_fun)
})

test_that("can use pdfs", {
  vcr::local_cassette("anthropic-pdfs")
  chat_fun <- chat_anthropic_test

  test_pdf_local(chat_fun)
})

# Custom features --------------------------------------------------------

test_that("can set beta headers", {
  chat <- chat_anthropic_test(beta_headers = c("a", "b"))
  req <- chat_request(chat$get_provider())
  headers <- req_get_headers(req)
  expect_equal(headers$`anthropic-beta`, "a,b")
})

test_that("continues to work after whitespace only outputs (#376)", {
  vcr::local_cassette("anthropic-whitespace")

  chat <- chat_anthropic_test()
  chat$chat("Respond with only two blank lines")
  expect_equal(
    chat$chat("What's 1+1? Just give me the number"),
    ellmer_output("2")
  )
})

test_that("can match prices for some common models", {
  provider <- chat_anthropic_test()$get_provider()

  expect_true(has_cost(provider, "claude-sonnet-4-20250514"))
})

test_that("removes empty final chat messages", {
  chat <- chat_anthropic_test()
  chat$set_turns(
    list(
      UserTurn("Don't say anything"),
      AssistantTurn()
    )
  )

  turns_json <- as_json(chat$get_provider(), chat$get_turns())

  expect_length(turns_json, 1)
  expect_equal(turns_json[[1]]$role, "user")
  expect_equal(
    turns_json[[1]]$content,
    list(list(type = "text", text = "Don't say anything"))
  )
})

test_that("batch chat works", {
  chat <- chat_anthropic_test(system_prompt = "Answer with just the city name")

  prompts <- list(
    "What's the capital of Iowa?",
    "What's the capital of New York?",
    "What's the capital of California?",
    "What's the capital of Texas?"
  )

  out <- batch_chat_text(
    chat,
    prompts,
    path = test_path("batch/state-capitals-anthropic.json")
  )
  expect_equal(out, c("Des Moines", "Albany", "Sacramento", "Austin"))
})

test_that("value_turn() parses server_tool_use input from JSON string", {
  provider <- test_anthropic_provider()

  result <- list(
    content = list(
      list(
        type = "server_tool_use",
        id = "srvtoolu_1",
        name = "web_search",
        input = '{"query":"test search"}'
      )
    ),
    stop_reason = "end_turn",
    usage = list(
      input_tokens = 10,
      output_tokens = 5,
      cache_creation_input_tokens = 0,
      cache_read_input_tokens = 0
    )
  )

  turn <- value_turn(provider, result)
  search_content <- turn@contents[[1]]
  expect_s7_class(search_content, ContentToolRequestSearch)
  expect_equal(search_content@query, "test search")
})

test_that("value_turn() parses server_tool_use web_fetch input from JSON string", {
  provider <- test_anthropic_provider()

  result <- list(
    content = list(
      list(
        type = "server_tool_use",
        id = "srvtoolu_2",
        name = "web_fetch",
        input = '{"url":"https://example.com"}'
      )
    ),
    stop_reason = "end_turn",
    usage = list(
      input_tokens = 10,
      output_tokens = 5,
      cache_creation_input_tokens = 0,
      cache_read_input_tokens = 0
    )
  )

  turn <- value_turn(provider, result)
  fetch_content <- turn@contents[[1]]
  expect_s7_class(fetch_content, ContentToolRequestFetch)
  expect_equal(fetch_content@url, "https://example.com")
})

test_that("value_turn() parses mcp_tool_use content", {
  provider <- test_anthropic_provider()

  result <- list(
    content = list(
      list(
        type = "mcp_tool_use",
        id = "mcptoolu_1",
        name = "execute_query",
        server_name = "snowflake",
        input = list(query = "SHOW DATABASES")
      )
    ),
    stop_reason = "end_turn",
    usage = list(
      input_tokens = 10,
      output_tokens = 5,
      cache_creation_input_tokens = 0,
      cache_read_input_tokens = 0
    )
  )

  turn <- value_turn(provider, result)
  mcp_content <- turn@contents[[1]]
  expect_s7_class(mcp_content, ContentMcpToolRequest)
  expect_equal(mcp_content@id, "mcptoolu_1")
  expect_equal(mcp_content@name, "execute_query")
  expect_equal(mcp_content@server_name, "snowflake")
  expect_equal(mcp_content@json$input, list(query = "SHOW DATABASES"))
})

test_that("value_turn() parses mcp_tool_use with string input from streaming", {
  provider <- test_anthropic_provider()

  result <- list(
    content = list(
      list(
        type = "mcp_tool_use",
        id = "mcptoolu_2",
        name = "execute_query",
        server_name = "snowflake",
        input = '{"query": "SHOW DATABASES"}'
      )
    ),
    stop_reason = "end_turn",
    usage = list(
      input_tokens = 10,
      output_tokens = 5,
      cache_creation_input_tokens = 0,
      cache_read_input_tokens = 0
    )
  )

  turn <- value_turn(provider, result)
  mcp_content <- turn@contents[[1]]
  expect_s7_class(mcp_content, ContentMcpToolRequest)
  expect_equal(mcp_content@arguments, list(query = "SHOW DATABASES"))
  expect_equal(mcp_content@json$input, list(query = "SHOW DATABASES"))
})

test_that("value_turn() parses mcp_tool_use with empty string input from streaming", {
  provider <- test_anthropic_provider()

  result <- list(
    content = list(
      list(
        type = "mcp_tool_use",
        id = "mcptoolu_3",
        name = "list_tools",
        server_name = "snowflake",
        input = "{}"
      )
    ),
    stop_reason = "end_turn",
    usage = list(
      input_tokens = 10,
      output_tokens = 5,
      cache_creation_input_tokens = 0,
      cache_read_input_tokens = 0
    )
  )

  turn <- value_turn(provider, result)
  mcp_content <- turn@contents[[1]]
  expect_s7_class(mcp_content, ContentMcpToolRequest)
  expect_type(mcp_content@json$input, "list")
})

test_that("value_turn() parses mcp_tool_result content", {
  provider <- test_anthropic_provider()

  result <- list(
    content = list(
      list(
        type = "mcp_tool_result",
        tool_use_id = "mcptoolu_1",
        content = list(list(type = "text", text = "DB1\nDB2")),
        is_error = FALSE
      )
    ),
    stop_reason = "end_turn",
    usage = list(
      input_tokens = 10,
      output_tokens = 5,
      cache_creation_input_tokens = 0,
      cache_read_input_tokens = 0
    )
  )

  turn <- value_turn(provider, result)
  mcp_content <- turn@contents[[1]]
  expect_s7_class(mcp_content, ContentMcpToolResult)
  expect_s7_class(mcp_content, ContentToolResult)
  expect_equal(mcp_content@request@id, "mcptoolu_1")
  expect_null(mcp_content@error)
  expect_equal(mcp_content@value, "DB1\nDB2")
  expect_equal(
    mcp_content@content,
    list(list(type = "text", text = "DB1\nDB2"))
  )
})

test_that("value_turn() links mcp_tool_result to its mcp_tool_use request", {
  provider <- test_anthropic_provider()

  result <- list(
    content = list(
      list(
        type = "mcp_tool_use",
        id = "mcptoolu_1",
        name = "execute_query",
        server_name = "snowflake",
        input = list(query = "SHOW DATABASES")
      ),
      list(
        type = "mcp_tool_result",
        tool_use_id = "mcptoolu_1",
        content = list(list(type = "text", text = "DB1\nDB2")),
        is_error = FALSE
      )
    ),
    stop_reason = "end_turn",
    usage = list(
      input_tokens = 10,
      output_tokens = 5,
      cache_creation_input_tokens = 0,
      cache_read_input_tokens = 0
    )
  )

  turn <- value_turn(provider, result)
  mcp_result <- turn@contents[[2]]
  expect_s7_class(mcp_result, ContentMcpToolResult)
  expect_equal(mcp_result@request@id, "mcptoolu_1")
  expect_equal(mcp_result@request@name, "execute_query")
  expect_equal(mcp_result@request@arguments, list(query = "SHOW DATABASES"))
})

test_that("value_turn() extracts images from mcp_tool_result content", {
  provider <- test_anthropic_provider()

  result <- list(
    content = list(
      list(
        type = "mcp_tool_result",
        tool_use_id = "mcptoolu_1",
        content = list(
          list(type = "text", text = "Here is the chart:"),
          list(
            type = "image",
            source = list(
              type = "base64",
              media_type = "image/png",
              data = "iVBORw0KGgo="
            )
          )
        ),
        is_error = FALSE
      )
    ),
    stop_reason = "end_turn",
    usage = list(
      input_tokens = 10,
      output_tokens = 5,
      cache_creation_input_tokens = 0,
      cache_read_input_tokens = 0
    )
  )

  turn <- value_turn(provider, result)
  expect_length(turn@contents, 2)

  mcp_result <- turn@contents[[1]]
  expect_s7_class(mcp_result, ContentMcpToolResult)
  expect_equal(mcp_result@value, "Here is the chart:")

  img <- turn@contents[[2]]
  expect_s7_class(img, ContentImageInline)
  expect_equal(img@type, "image/png")
  expect_equal(img@data, "iVBORw0KGgo=")
})

test_that("value_turn() parses mcp_tool_result with is_error = TRUE", {
  provider <- test_anthropic_provider()

  result <- list(
    content = list(
      list(
        type = "mcp_tool_result",
        tool_use_id = "mcptoolu_2",
        content = list(list(type = "text", text = "Connection refused")),
        is_error = TRUE
      )
    ),
    stop_reason = "end_turn",
    usage = list(
      input_tokens = 10,
      output_tokens = 5,
      cache_creation_input_tokens = 0,
      cache_read_input_tokens = 0
    )
  )

  turn <- value_turn(provider, result)
  mcp_content <- turn@contents[[1]]
  expect_s7_class(mcp_content, ContentMcpToolResult)
  expect_equal(mcp_content@error, "Connection refused")
  expect_equal(
    mcp_content@content,
    list(list(type = "text", text = "Connection refused"))
  )
})

test_that("format(ContentMcpToolRequest) shows ID, server/name, and arguments", {
  req <- ContentMcpToolRequest(
    id = "mcptoolu_1",
    name = "execute_query",
    arguments = list(query = "SHOW DATABASES"),
    tool = mcp_tool_def("execute_query", "snowflake"),
    server_name = "snowflake",
    json = list(input = list(query = "SHOW DATABASES"))
  )
  out <- format(req)
  expect_match(out, "mcp tool request")
  expect_match(out, "mcptoolu_1", fixed = TRUE)
  expect_match(out, "snowflake__execute_query", fixed = TRUE)
  expect_match(out, "SHOW DATABASES", fixed = TRUE)
})

test_that("format(ContentMcpToolRequest) handles non-list input", {
  req <- ContentMcpToolRequest(
    id = "mcptoolu_2",
    name = "echo",
    arguments = list("hello world"),
    tool = mcp_tool_def("echo", "test"),
    server_name = "test",
    json = list(input = "hello world")
  )
  out <- format(req)
  expect_match(out, "mcp tool request")
  expect_match(out, "mcptoolu_2", fixed = TRUE)
  expect_match(out, "hello world", fixed = TRUE)
})

test_that("format(ContentMcpToolRequest) handles missing input", {
  req <- ContentMcpToolRequest(
    id = "mcptoolu_3",
    name = "list_tools",
    arguments = list(),
    tool = mcp_tool_def("list_tools", "test"),
    server_name = "test",
    json = list()
  )
  out <- format(req)
  expect_match(out, "mcp tool request")
  expect_match(out, "test__list_tools", fixed = TRUE)
})

test_that("format(ContentMcpToolResult) shows ID and result text", {
  res <- ContentMcpToolResult(
    value = "DB1\nDB2",
    request = ContentToolRequest(
      id = "mcptoolu_1",
      name = "",
      arguments = list()
    ),
    content = list(list(type = "text", text = "DB1\nDB2")),
    json = list()
  )
  out <- format(res)
  expect_match(out, "mcp tool result")
  expect_match(out, "mcptoolu_1", fixed = TRUE)
  expect_match(out, "DB1", fixed = TRUE)
})

test_that("format(ContentMcpToolResult) shows error in red", {
  res <- ContentMcpToolResult(
    error = "Connection refused",
    request = ContentToolRequest(
      id = "mcptoolu_2",
      name = "",
      arguments = list()
    ),
    content = list(list(type = "text", text = "Connection refused")),
    json = list()
  )
  out <- format(res)
  expect_match(out, "mcp tool result")
  expect_match(out, "mcptoolu_2", fixed = TRUE)
  expect_match(out, "Connection refused", fixed = TRUE)
})

test_that("format(ContentMcpToolResult) handles empty content", {
  res <- ContentMcpToolResult(
    value = "",
    request = ContentToolRequest(
      id = "mcptoolu_3",
      name = "",
      arguments = list()
    ),
    content = list(),
    json = list()
  )
  expect_no_error(format(res))
})

test_that("value_turn() prices cache writes at 1.25x while reporting raw tokens", {
  provider <- test_anthropic_provider()

  result <- list(
    content = list(list(type = "text", text = "ok")),
    stop_reason = "end_turn",
    usage = list(
      input_tokens = 1000,
      output_tokens = 50,
      cache_creation_input_tokens = 400,
      cache_read_input_tokens = 200
    )
  )

  turn <- value_turn(provider, result)

  # tokens slot reports raw integer counts (no 1.25x weighting on input).
  expect_equal(
    unname(turn@tokens),
    c(1000 + 400, 50, 200)
  )

  # Cost matches the 1.25x cache-write weighting:
  #   (1000 + 400 * 1.25) * $3/1M + 50 * $15/1M + 200 * $0.30/1M
  expected_cost <- ((1000 + 400 * 1.25) * 3 + 50 * 15 + 200 * 0.30) / 1e6
  expect_equal(unclass(turn@cost), expected_cost)
})

test_that("stream_merge_chunks() handles citations_delta", {
  provider <- test_anthropic_provider()

  chunks <- list(
    list(
      type = "message_start",
      message = list(
        id = "msg_1",
        type = "message",
        role = "assistant",
        content = list(),
        model = "claude-sonnet-4-20250514",
        stop_reason = NULL,
        stop_sequence = NULL,
        usage = list(input_tokens = 10, output_tokens = 1)
      )
    ),
    list(
      type = "content_block_start",
      index = 0L,
      content_block = list(
        type = "text",
        text = ""
      )
    ),
    list(
      type = "content_block_delta",
      index = 0L,
      delta = list(
        type = "text_delta",
        text = "Hello"
      )
    ),
    list(
      type = "content_block_delta",
      index = 0L,
      delta = list(
        type = "citations_delta",
        citation = list(
          type = "web_search_result_location",
          cited_text = "example text",
          url = "https://example.com"
        )
      )
    ),
    list(type = "content_block_stop", index = 0L),
    list(
      type = "message_delta",
      delta = list(
        stop_reason = "end_turn",
        stop_sequence = NULL
      ),
      usage = list(output_tokens = 5)
    )
  )

  result <- NULL
  expect_no_warning({
    for (chunk in chunks) {
      result <- stream_merge_chunks(provider, result, chunk)
    }
  })

  expect_equal(result$content[[1]]$text, "Hello")
  expect_length(result$content[[1]]$citations, 1)
  expect_equal(result$content[[1]]$citations[[1]]$url, "https://example.com")
})
