test_that("mcp_connector() creates McpConnector object", {
  conn <- mcp_connector("https://example.com/mcp", name = "test")
  expect_s3_class(conn, "ellmer::McpConnector")
  expect_equal(conn@url, "https://example.com/mcp")
  expect_equal(conn@name, "test")
  expect_null(conn@credentials)
  expect_equal(conn@extra, list())
})

test_that("mcp_connector() validates inputs", {
  expect_error(mcp_connector(1, name = "test"))
  expect_error(mcp_connector("url", name = 1))
})

test_that("mcp_connector() accepts credentials function", {
  creds <- function() "my-token"
  conn <- mcp_connector(
    "https://example.com/mcp",
    name = "test",
    credentials = creds
  )
  expect_equal(conn@credentials(), "my-token")
})

test_that("mcp_connector() captures extra args", {
  conn <- mcp_connector(
    "https://example.com/mcp",
    name = "test",
    require_approval = "never"
  )
  expect_equal(conn@extra, list(require_approval = "never"))
})

test_that("register_tool() rejects McpConnector for unsupported providers", {
  chat <- Chat$new(test_provider())
  expect_snapshot(error = TRUE, {
    chat$register_tool(
      mcp_connector("https://example.com/mcp", name = "test")
    )
  })
})

test_that("register_tool() accepts McpConnector for OpenAI", {
  provider <- ProviderOpenAI(
    name = "OpenAI",
    base_url = "https://api.openai.com/v1",
    model = "gpt-4.1",
    params = list(),
    extra_args = list(),
    extra_headers = character(),
    credentials = NULL,
    service_tier = "auto"
  )
  chat <- Chat$new(provider = provider)
  chat$register_tool(mcp_connector("https://example.com/mcp", name = "test"))
  expect_length(chat$get_tools(), 1)
})

test_that("as_json(ProviderOpenAI, McpConnector) returns MCP tool entry", {
  provider <- ProviderOpenAI(
    name = "OpenAI",
    base_url = "https://api.openai.com/v1",
    model = "gpt-4.1",
    params = list(),
    extra_args = list(),
    extra_headers = character(),
    credentials = NULL,
    service_tier = "auto"
  )
  conn <- mcp_connector("https://example.com/mcp", name = "test")
  json <- as_json(provider, conn)
  expect_equal(json$type, "mcp")
  expect_equal(json$server_label, "test")
  expect_equal(json$server_url, "https://example.com/mcp")
  expect_null(json$authorization)
})

test_that("as_json(ProviderOpenAI, McpConnector) includes credentials", {
  provider <- ProviderOpenAI(
    name = "OpenAI",
    base_url = "https://api.openai.com/v1",
    model = "gpt-4.1",
    params = list(),
    extra_args = list(),
    extra_headers = character(),
    credentials = NULL,
    service_tier = "auto"
  )
  conn <- mcp_connector(
    "https://example.com/mcp",
    name = "test",
    credentials = function() "Bearer my-token"
  )
  json <- as_json(provider, conn)
  expect_equal(json$authorization, "Bearer my-token")
})

test_that("as_json(ProviderOpenAI, McpConnector) passes extra args", {
  provider <- ProviderOpenAI(
    name = "OpenAI",
    base_url = "https://api.openai.com/v1",
    model = "gpt-4.1",
    params = list(),
    extra_args = list(),
    extra_headers = character(),
    credentials = NULL,
    service_tier = "auto"
  )
  conn <- mcp_connector(
    "https://example.com/mcp",
    name = "test",
    require_approval = "never",
    allowed_tools = list("tool1", "tool2")
  )
  json <- as_json(provider, conn)
  expect_equal(json$require_approval, "never")
  expect_equal(json$allowed_tools, list("tool1", "tool2"))
})

# Anthropic provider -----------------------------------------------------------

test_that("register_tool() accepts McpConnector for Anthropic", {
  provider <- ProviderAnthropic(
    name = "Anthropic",
    base_url = "https://api.anthropic.com/v1",
    model = "claude-sonnet-4-5-20250929",
    params = list(),
    extra_args = list(),
    extra_headers = character(),
    credentials = NULL,
    beta_headers = character(),
    cache = ""
  )
  chat <- Chat$new(provider = provider)
  chat$register_tool(mcp_connector("https://example.com/mcp", name = "test"))
  expect_length(chat$get_tools(), 1)
})

test_that("as_json(ProviderAnthropic, McpConnector) returns mcp_toolset", {
  provider <- ProviderAnthropic(
    name = "Anthropic",
    base_url = "https://api.anthropic.com/v1",
    model = "claude-sonnet-4-5-20250929",
    params = list(),
    extra_args = list(),
    extra_headers = character(),
    credentials = NULL,
    beta_headers = character(),
    cache = ""
  )
  conn <- mcp_connector("https://example.com/mcp", name = "test")
  json <- as_json(provider, conn)
  expect_equal(json, list(type = "mcp_toolset", mcp_server_name = "test"))
})

test_that("chat_body(ProviderAnthropic) includes mcp_servers", {
  provider <- ProviderAnthropic(
    name = "Anthropic",
    base_url = "https://api.anthropic.com/v1",
    model = "claude-sonnet-4-5-20250929",
    params = list(),
    extra_args = list(),
    extra_headers = character(),
    credentials = NULL,
    beta_headers = character(),
    cache = ""
  )
  conn <- mcp_connector("https://example.com/mcp", name = "test")
  body <- chat_body(provider, stream = TRUE, turns = list(), tools = list(conn))
  expect_equal(
    body$mcp_servers,
    list(list(type = "url", url = "https://example.com/mcp", name = "test"))
  )
  expect_equal(
    body$tools,
    list(list(type = "mcp_toolset", mcp_server_name = "test"))
  )
})

test_that("chat_body(ProviderAnthropic) includes credentials in mcp_servers", {
  provider <- ProviderAnthropic(
    name = "Anthropic",
    base_url = "https://api.anthropic.com/v1",
    model = "claude-sonnet-4-5-20250929",
    params = list(),
    extra_args = list(),
    extra_headers = character(),
    credentials = NULL,
    beta_headers = character(),
    cache = ""
  )
  conn <- mcp_connector(
    "https://example.com/mcp",
    name = "test",
    credentials = function() "my-token"
  )
  body <- chat_body(provider, stream = TRUE, turns = list(), tools = list(conn))
  expect_equal(body$mcp_servers[[1]]$authorization_token, "my-token")
})

test_that("chat_request(ProviderAnthropic) adds MCP beta header", {
  provider <- ProviderAnthropic(
    name = "Anthropic",
    base_url = "https://api.anthropic.com/v1",
    model = "claude-sonnet-4-5-20250929",
    params = list(),
    extra_args = list(),
    extra_headers = character(),
    credentials = function() "fake-key",
    beta_headers = character(),
    cache = ""
  )
  conn <- mcp_connector("https://example.com/mcp", name = "test")
  req <- chat_request(
    provider,
    stream = TRUE,
    turns = list(),
    tools = list(conn)
  )
  headers <- httr2::req_get_headers(req)
  expect_match(headers$`anthropic-beta`, "mcp-client-2025-11-20")
})

test_that("chat_request(ProviderAnthropic) merges MCP beta with existing", {
  provider <- ProviderAnthropic(
    name = "Anthropic",
    base_url = "https://api.anthropic.com/v1",
    model = "claude-sonnet-4-5-20250929",
    params = list(),
    extra_args = list(),
    extra_headers = character(),
    credentials = function() "fake-key",
    beta_headers = "existing-beta",
    cache = ""
  )
  conn <- mcp_connector("https://example.com/mcp", name = "test")
  req <- chat_request(
    provider,
    stream = TRUE,
    turns = list(),
    tools = list(conn)
  )
  headers <- httr2::req_get_headers(req)
  expect_match(headers$`anthropic-beta`, "existing-beta")
  expect_match(headers$`anthropic-beta`, "mcp-client-2025-11-20")
})

# MCP content inheritance -------------------------------------------------------

test_that("ContentMcpToolRequest inherits from ContentToolRequest", {
  req <- ContentMcpToolRequest(
    id = "mcp_1",
    name = "search",
    arguments = list(query = "test"),
    tool = mcp_tool_def("search", "server"),
    server_name = "server",
    json = list()
  )
  expect_s7_class(req, ContentMcpToolRequest)
  expect_s7_class(req, ContentToolRequest)
  expect_equal(req@id, "mcp_1")
  expect_equal(req@name, "search")
  expect_equal(req@arguments, list(query = "test"))
  expect_equal(req@server_name, "server")
})

test_that("ContentMcpToolResult inherits from ContentToolResult", {
  res <- ContentMcpToolResult(
    value = "result text",
    request = ContentToolRequest(id = "mcp_1", name = "", arguments = list()),
    content = list(list(type = "text", text = "result text")),
    json = list()
  )
  expect_s7_class(res, ContentMcpToolResult)
  expect_s7_class(res, ContentToolResult)
  expect_equal(res@value, "result text")
  expect_null(res@error)
  expect_equal(res@request@id, "mcp_1")
})

test_that("is_tool_request() excludes MCP by default", {
  local_req <- ContentToolRequest(
    id = "local_1",
    name = "fn",
    arguments = list()
  )
  mcp_req <- ContentMcpToolRequest(
    id = "mcp_1",
    name = "search",
    arguments = list(),
    tool = mcp_tool_def("search", "server"),
    server_name = "server",
    json = list()
  )
  expect_true(is_tool_request(local_req))
  expect_false(is_tool_request(mcp_req))
  expect_true(is_tool_request(mcp_req, local_only = FALSE))
})

test_that("is_tool_result() excludes MCP by default", {
  local_res <- ContentToolResult(
    value = "ok",
    request = ContentToolRequest(id = "1", name = "fn", arguments = list())
  )

  mcp_res <- ContentMcpToolResult(
    value = "ok",
    request = ContentToolRequest(id = "1", name = "", arguments = list()),
    content = list(list(type = "text", text = "ok")),
    json = list()
  )
  expect_true(is_tool_result(local_res))
  expect_false(is_tool_result(mcp_res))
  expect_true(is_tool_result(mcp_res, local_only = FALSE))
})

test_that("mcp_tool_def() creates a ToolDef with no-op function", {
  td <- mcp_tool_def("query", "myserver")
  expect_s7_class(td, ToolDef)
  expect_equal(td@name, "myserver__query")
  expect_invisible(td())
})

test_that("maybe_echo_tool() echoes MCP content", {
  req <- ContentMcpToolRequest(
    id = "mcp_1",
    name = "search",
    arguments = list(query = "test"),
    tool = mcp_tool_def("search", "server"),
    server_name = "server",
    json = list()
  )
  out <- capture.output(maybe_echo_tool(req, echo = "output"), type = "message")
  expect_match(paste(out, collapse = ""), "server__search")

  res <- ContentMcpToolResult(
    value = "found it",
    request = ContentToolRequest(id = "mcp_1", name = "", arguments = list()),
    content = list(list(type = "text", text = "found it")),
    json = list()
  )
  out <- capture.output(maybe_echo_tool(res, echo = "output"), type = "message")
  expect_match(paste(out, collapse = ""), "found it")
})
