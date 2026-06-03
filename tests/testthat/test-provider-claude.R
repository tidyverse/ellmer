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

test_that("can make simple batch request", {
  vcr::local_cassette("anthropic-batch")

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

test_that("value_turn() parses code execution requests", {
  provider <- test_anthropic_provider()

  result <- list(
    content = list(
      list(
        type = "server_tool_use",
        id = "srvtoolu_1",
        name = "bash_code_execution",
        input = '{"command":"ls -la"}'
      ),
      list(
        type = "server_tool_use",
        id = "srvtoolu_2",
        name = "text_editor_code_execution",
        input = '{"command":"create","path":"a.txt","file_text":"hi"}'
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
  expect_s7_class(turn@contents[[1]], ContentToolRequestCode)
  expect_s7_class(turn@contents[[1]], ContentToolRequest)
  expect_equal(turn@contents[[1]]@name, "bash_code_execution")
  expect_equal(turn@contents[[1]]@arguments, list(command = "ls -la"))
  expect_match(format(turn@contents[[1]]), "ls -la")
  expect_s7_class(turn@contents[[2]], ContentToolRequestCode)
  expect_equal(turn@contents[[2]]@name, "text_editor_code_execution")
  expect_match(format(turn@contents[[2]]), "a.txt")
})

test_that("value_turn() links code execution results to their requests", {
  provider <- test_anthropic_provider()

  result <- list(
    content = list(
      list(
        type = "server_tool_use",
        id = "srvtoolu_1",
        name = "bash_code_execution",
        input = '{"command":"echo hi"}'
      ),
      list(
        type = "bash_code_execution_tool_result",
        tool_use_id = "srvtoolu_1",
        content = list(
          type = "bash_code_execution_result",
          stdout = "hi",
          stderr = "",
          return_code = 0
        )
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
  res <- turn@contents[[2]]
  expect_s7_class(res, ContentToolResponseCode)
  expect_s7_class(res, ContentToolResult)
  expect_equal(res@request@name, "bash_code_execution")
  expect_equal(res@request@arguments, list(command = "echo hi"))
  expect_match(res@value, "hi")
})

test_that("value_turn() parses code execution results", {
  provider <- test_anthropic_provider()

  result <- list(
    content = list(
      list(
        type = "bash_code_execution_tool_result",
        tool_use_id = "srvtoolu_1",
        content = list(
          type = "bash_code_execution_result",
          stdout = "hello world",
          stderr = "",
          return_code = 0
        )
      ),
      list(
        type = "bash_code_execution_tool_result",
        tool_use_id = "srvtoolu_2",
        content = list(
          type = "bash_code_execution_tool_result_error",
          error_code = "unavailable"
        )
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
  expect_s7_class(turn@contents[[1]], ContentToolResponseCode)
  expect_match(format(turn@contents[[1]]), "hello world")
  expect_s7_class(turn@contents[[2]], ContentToolResponseCode)
  expect_match(format(turn@contents[[2]]), "unavailable")
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

# extra_args$tools merging ---------------------------------------------------

test_that("extra_args$tools are preserved when no function tools registered", {
  provider <- ProviderAnthropic(
    name = "Anthropic",
    base_url = "https://api.anthropic.com/v1",
    model = "claude-sonnet-4-5-20250929",
    params = list(),
    extra_args = list(tools = list(list(type = "mcp_toolset", mcp_server_name = "test"))),
    extra_headers = character(),
    credentials = function() "fake-key",
    beta_headers = character(),
    cache = ""
  )

  req <- chat_request(provider, stream = FALSE, turns = list(), tools = list())
  body_tools <- req$body$data$tools
  types <- vapply(body_tools, function(t) t$type %||% "", character(1))
  expect_true("mcp_toolset" %in% types)
})

test_that("as_json() emits allowed_callers for Anthropic tools", {
  provider <- test_anthropic_provider()
  t <- tool(
    function() 1,
    name = "f",
    description = "d",
    allowed_callers = c("direct", "code_execution_20260120")
  )
  json <- as_json(provider, t)
  expect_equal(json$allowed_callers, list("direct", "code_execution_20260120"))
})

test_that("as_json() omits allowed_callers when unset", {
  provider <- test_anthropic_provider()
  t <- tool(function() 1, name = "f", description = "d")
  json <- as_json(provider, t)
  expect_null(json$allowed_callers)
})

test_that("extra_args$tools are preserved alongside registered function tools", {
  provider <- ProviderAnthropic(
    name = "Anthropic",
    base_url = "https://api.anthropic.com/v1",
    model = "claude-sonnet-4-5-20250929",
    params = list(),
    extra_args = list(tools = list(list(type = "mcp_toolset", mcp_server_name = "test"))),
    extra_headers = character(),
    credentials = function() "fake-key",
    beta_headers = character(),
    cache = ""
  )

  my_tool <- tool(function() 1, "A test tool", name = "my_func")
  req <- chat_request(
    provider,
    stream = FALSE,
    turns = list(),
    tools = list(my_func = my_tool)
  )
  body_tools <- req$body$data$tools

  types <- vapply(body_tools, function(t) t$type %||% "", character(1))
  names_vec <- vapply(body_tools, function(t) t$name %||% "", character(1))

  expect_true("mcp_toolset" %in% types)
  expect_true("my_func" %in% names_vec)
})

test_that("value_turn() round-trips the caller field on programmatic tool calls", {
  provider <- test_anthropic_provider()
  result <- list(
    content = list(
      list(
        type = "tool_use",
        id = "toolu_1",
        name = "query_database",
        input = list(sql = "SELECT 1"),
        caller = list(
          type = "code_execution_20260120",
          tool_id = "srvtoolu_9"
        )
      )
    ),
    stop_reason = "tool_use",
    usage = list(
      input_tokens = 1,
      output_tokens = 1,
      cache_creation_input_tokens = 0,
      cache_read_input_tokens = 0
    )
  )

  turn <- value_turn(provider, result)
  req <- turn@contents[[1]]
  expect_s7_class(req, ContentToolRequest)
  expect_equal(
    req@extra$caller,
    list(type = "code_execution_20260120", tool_id = "srvtoolu_9")
  )

  json <- as_json(provider, req)
  expect_equal(json$type, "tool_use")
  expect_equal(
    json$caller,
    list(type = "code_execution_20260120", tool_id = "srvtoolu_9")
  )
})

test_that("direct tool calls serialize without a caller field", {
  provider <- test_anthropic_provider()
  req <- ContentToolRequest("toolu_1", "f", list(x = 1))
  json <- as_json(provider, req)
  expect_null(json$caller)
})

test_that("tool requests keep an empty input for argument-free tools", {
  provider <- test_anthropic_provider()
  req <- ContentToolRequest("toolu_1", "f", list())
  json <- as_json(provider, req)
  expect_equal(json$input, list())
})

test_that("last_container_id() returns the newest container id in history", {
  turns <- list(
    UserTurn("hi"),
    AssistantTurn(
      list(ContentText("a")),
      json = list(container = list(id = "container_1"))
    ),
    UserTurn("more"),
    AssistantTurn(
      list(ContentText("b")),
      json = list(container = list(id = "container_2"))
    )
  )
  expect_equal(last_container_id(turns), "container_2")
})

test_that("last_container_id() returns NULL when no container is present", {
  turns <- list(UserTurn("hi"), AssistantTurn(list(ContentText("a"))))
  expect_null(last_container_id(turns))
})

test_that("chat_body() reuses the conversation's container id", {
  provider <- test_anthropic_provider()
  turns <- list(
    UserTurn("hi"),
    AssistantTurn(
      list(ContentText("a")),
      json = list(container = list(id = "container_1"))
    )
  )
  body <- chat_body(provider, turns = turns)
  expect_equal(body$container, "container_1")
})

test_that("chat_body() omits container when none has been created", {
  provider <- test_anthropic_provider()
  body <- chat_body(provider, turns = list(UserTurn("hi")))
  expect_null(body$container)
})

test_that("chat_body() warns when allowed_callers is set but no code tool is registered", {
  withr::local_options(rlib_warning_verbosity = "verbose")
  provider <- test_anthropic_provider()
  programmatic <- tool(
    function() 1,
    name = "f",
    description = "d",
    allowed_callers = "code_execution_20260120"
  )
  expect_snapshot(
    . <- chat_body(
      provider,
      turns = list(UserTurn("hi")),
      tools = list(f = programmatic)
    )
  )
})

test_that("chat_body() does not warn when the code execution tool is registered", {
  provider <- test_anthropic_provider()
  programmatic <- tool(
    function() 1,
    name = "f",
    description = "d",
    allowed_callers = "code_execution_20260120"
  )
  code_tool <- claude_tool_code_execution(type = "code_execution_20260120")
  expect_no_warning(
    chat_body(
      provider,
      turns = list(UserTurn("hi")),
      tools = list(f = programmatic, code_execution = code_tool)
    )
  )
})

test_that("chat_body() does not warn for plain tools without allowed_callers", {
  provider <- test_anthropic_provider()
  plain <- tool(function() 1, name = "f", description = "d")
  expect_no_warning(
    chat_body(provider, turns = list(UserTurn("hi")), tools = list(f = plain))
  )
})
