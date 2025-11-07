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
  ToolBuiltIn("web_search", json = json)
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
  ToolBuiltIn("web_fetch", json)
}
