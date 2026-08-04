McpConnector <- new_class(
  "McpConnector",
  properties = list(
    name = prop_string(),
    url = prop_string(),
    credentials = class_function | NULL,
    extra = class_list
  )
)

#' MCP connector for provider-hosted MCP servers
#'
#' @description
#' Creates a connector for a provider-hosted
#' [Model Context Protocol](https://modelcontextprotocol.io/) (MCP) server.
#' With an MCP connector, the LLM provider connects to the MCP server on
#' your behalf, giving the model access to tools provided by the server.
#'
#' Currently, [chat_anthropic()] and [chat_openai()] support MCP connectors.
#' See the provider documentation for provider-specific details.
#'
#' @param url The URL of the MCP server.
#' @param name A label for the MCP server connection.
#' @param credentials A zero-argument function that returns an auth token
#'   for the MCP server, or `NULL` (the default) for no authentication.
#'   The return value is sent as `authorization_token` (Anthropic) or
#'   `authorization` (OpenAI) in the MCP server configuration.
#' @param ... Additional provider-specific arguments passed to the MCP
#'   server configuration.
#'
#' @return An S7 `McpConnector` object.
#'
#' @family built-in tools
#' @export
#' @examples
#' \dontrun{
#' chat <- chat_anthropic()
#' chat$register_tool(mcp_connector(
#'   url = "https://mcp.deepwiki.com/mcp",
#'   name = "deepwiki"
#' ))
#' chat$chat("What is ellmer?")
#' }
mcp_connector <- function(url, name, credentials = NULL, ...) {
  check_string(url)
  check_string(name)

  if (!is.null(credentials)) {
    check_credentials(credentials)
  }

  McpConnector(
    name = name,
    url = url,
    credentials = credentials,
    extra = list2(...)
  )
}
