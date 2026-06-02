#' Claude web search tool
#'
#' @description
#' Enables Claude to search the web for up-to-date information. Your organization
#' administrator must enable web search in the Anthropic Console before using
#' this tool, as it costs extra ($10 per 1,000 tokens at time of writing).
#'
#' Learn more in <https://docs.claude.com/en/docs/agents-and-tools/tool-use/web-search-tool>.
#'
#' @param max_uses Integer. Maximum number of searches allowed per request.
#' @param allowed_domains Character vector. Restrict searches to specific domains
#'   (e.g., `c("nytimes.com", "bbc.com")`). Cannot be used with `blocked_domains`.
#' @param blocked_domains Character vector. Exclude specific domains from searches.
#'   Cannot be used with `allowed_domains`.
#' @param user_location List with optional elements: `country` (2-letter code),
#'   `city`, `region`, and `timezone` (IANA timezone) to localize search results.
#'
#' @family built-in tools
#' @export
#' @examples
#' \dontrun{
#' chat <- chat_claude()
#' chat$register_tool(claude_tool_web_search())
#' chat$chat("What was in the news today?")
#' chat$chat("What's the biggest news in the economy?")
#' }
claude_tool_web_search <- function(
  max_uses = NULL,
  allowed_domains = NULL,
  blocked_domains = NULL,
  user_location = NULL
) {
  check_exclusive(allowed_domains, blocked_domains, .require = FALSE)

  check_number_whole(max_uses, allow_null = TRUE, min = 1)
  check_character(allowed_domains, allow_null = TRUE)
  check_character(blocked_domains, allow_null = TRUE)

  json <- compact(list(
    name = "web_search",
    type = "web_search_20250305",
    max_uses = max_uses,
    allowed_domains = allowed_domains,
    blocked_domains = blocked_domains,
    user_location = user_location
  ))
  ToolBuiltIn(
    name = "web_search",
    description = "Search the web for up-to-date information.",
    annotations = tool_annotations(
      title = "Web search",
      read_only_hint = TRUE,
      open_world_hint = TRUE
    ),
    json = json
  )
}

#' Claude web fetch tool
#'
#' @description
#' Enables Claude to fetch and analyze content from web URLs. Claude can only
#' fetch URLs that appear in the conversation context (user messages or
#' previous tool results). For security reasons, Claude cannot dynamically
#' construct URLs to fetch.
#'
#' Requires the `web-fetch-2025-09-10` beta header.
#' Learn more in <https://docs.claude.com/en/docs/agents-and-tools/tool-use/web-fetch-tool>.
#'
#' @param max_uses Integer. Maximum number of fetches allowed per request.
#' @param allowed_domains Character vector. Restrict fetches to specific domains.
#'   Cannot be used with `blocked_domains`.
#' @param blocked_domains Character vector. Exclude specific domains from fetches.
#'   Cannot be used with `allowed_domains`.
#' @param citations Logical. Whether to include citations in the response. Default is `TRUE`.
#' @param max_content_tokens Integer. Maximum number of tokens to fetch from each URL.
#'
#' @family built-in tools
#' @export
#' @examples
#' \dontrun{
#' chat <- chat_claude(beta_headers = "web-fetch-2025-09-10")
#' chat$register_tool(claude_tool_web_fetch())
#' chat$chat("What are the latest package releases on https://tidyverse.org/blog")
#' }
claude_tool_web_fetch <- function(
  max_uses = NULL,
  allowed_domains = NULL,
  blocked_domains = NULL,
  citations = FALSE,
  max_content_tokens = NULL
) {
  check_exclusive(allowed_domains, blocked_domains, .require = FALSE)

  check_character(allowed_domains, allow_null = TRUE)
  check_character(blocked_domains, allow_null = TRUE)
  check_bool(citations)

  json <- compact(list(
    name = "web_fetch",
    type = "web_fetch_20250910",
    max_uses = max_uses,
    allowed_domains = allowed_domains,
    blocked_domains = blocked_domains,
    citations = list(enabled = citations),
    max_content_tokens = max_content_tokens
  ))
  ToolBuiltIn(
    name = "web_fetch",
    description = "Fetch and analyze content from a web URL.",
    annotations = tool_annotations(
      title = "Web fetch",
      read_only_hint = TRUE,
      open_world_hint = TRUE
    ),
    json = json
  )
}

#' Claude code execution tool
#'
#' @description
#' Enables Claude to run Python and Bash code and manipulate files in a secure,
#' sandboxed container on Anthropic's servers. The container has no internet
#' access and is isolated from your machine; ellmer sends the tool spec and
#' displays the requests and results, but does not run any code locally.
#'
#' Some tool versions require a beta header. Anthropic changes these
#' frequently, so ellmer does not track them; pass any required header to
#' `chat_anthropic(beta_headers = ...)` yourself, e.g.
#' `beta_headers = "code-execution-2025-08-25"`.
#'
#' Learn more in <https://docs.claude.com/en/docs/agents-and-tools/tool-use/code-execution-tool>.
#'
#' @param type The code execution tool `type` to send to the API. Defaults to
#'   `"code_execution_20250825"` (Python, Bash, and file operations, supported
#'   on all current models). Set this to use a different version, such as
#'   `"code_execution_20250522"` (legacy, Python only) or a newer release;
#'   see the Anthropic documentation for the available versions and the models
#'   they support.
#'
#' @family built-in tools
#' @export
#' @examples
#' \dontrun{
#' chat <- chat_anthropic()
#' chat$register_tool(claude_tool_code_execution())
#' chat$chat("Calculate the mean and standard deviation of 1 to 10.")
#' }
claude_tool_code_execution <- function(type = "code_execution_20250825") {
  check_string(type)

  json <- list(
    type = type,
    name = "code_execution"
  )
  ToolBuiltIn(
    name = "code_execution",
    description = "Run Python and bash code in a sandboxed container.",
    annotations = tool_annotations(
      title = "Code execution",
      read_only_hint = FALSE,
      open_world_hint = FALSE
    ),
    json = json
  )
}
