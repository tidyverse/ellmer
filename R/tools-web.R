#' @include tools-built-in.R
NULL

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
#' chat <- chat_claude(model = "claude-sonnet-4-5-20250929")
#' chat$register_tool(tool_claude_web_search())
#' chat$chat("What was in the news today?")
#' }
tool_claude_web_search <- function(
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
  ToolBuiltIn("claude_web_search", json = json)
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
#' chat <- chat_claude(
#'   model = "claude-sonnet-4-5-20250929",
#'   beta_headers = "web-fetch-2025-09-10"
#' )
#' chat$register_tool(tool_claude_web_fetch())
#' chat$chat("Fetch and summarize https://tidyverse.org/blog")
#' }
tool_claude_web_fetch <- function(
  max_uses = NULL,
  allowed_domains = NULL,
  blocked_domains = NULL,
  citations = TRUE,
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
  ToolBuiltIn("claude_web_fetch", json)
}

# Google tools ----

#' Google web search (grounding) tool
#'
#' @description
#' Enables Gemini models to search the web for up-to-date information and ground
#' responses with citations to sources. The model automatically decides when
#' (and how) to search the web based on your prompt. Search results are
#' incorporated into the response with grounding metadata including source
#' URLs and titles.
#'
#' Learn more in <https://ai.google.dev/gemini-api/docs/google-search>.
#'
#' @family built-in tools
#' @export
#' @examples
#' \dontrun{
#' chat <- chat_google_gemini(model = "gemini-2.5-flash")
#' chat$register_tool(tool_google_web_search())
#' chat$chat("What's in the news today?")
#' }
tool_google_web_search <- function() {
  ToolBuiltIn("google_web_search", list(google_search = set_names(list())))
}

#' Google URL fetch tool
#'
#' @description
#' When this tool is enabled, you can include URLs directly in your prompts and
#' Gemini will fetch and analyze the content.
#'
#' Learn more in <https://ai.google.dev/gemini-api/docs/url-context>.
#'
#' @family built-in tools
#' @export
#' @examples
#' \dontrun{
#' chat <- chat_google_gemini(model = "gemini-2.5-flash")
#' chat$register_tool(tool_google_url_fetch())
#' chat$chat("Fetch and summarize https://tidyverse.org/blog")
#' }
tool_google_url_fetch <- function() {
  ToolBuiltIn(
    name = "google_url_context",
    json = list(url_context = set_names(list()))
  )
}


#' OpenAI web search tool
#'
#' @description
#' Enables OpenAI models to search the web for up-to-date information. The search
#' behavior varies by model: non-reasoning models perform simple searches, while
#' reasoning models can perform agentic, iterative searches.
#'
#' Learn more at <https://platform.openai.com/docs/guides/tools-web-search>
#'
#' @param allowed_domains Character vector. Restrict searches to specific domains
#'   (e.g., `c("nytimes.com", "bbc.com")`). Maximum 20 domains. URLs will be
#'   automatically cleaned (http/https prefixes removed).
#' @param user_location List with optional elements: `country` (2-letter ISO code),
#'   `city`, `region`, and `timezone` (IANA timezone) to localize search results.
#' @param external_web_access Logical. Whether to allow live internet access
#'   (`TRUE`, default) or use only cached/indexed results (`FALSE`).
#'
#' @family built-in tools
#' @export
#' @examples
#' \dontrun{
#' # Basic web search
#' chat <- chat_openai_responses(model = "gpt-5")
#' chat$register_tool(tool_openai_web_search())
#' chat$chat("What happened in the news today?")
#' }
tool_openai_web_search <- function(
  allowed_domains = NULL,
  user_location = NULL,
  external_web_access = TRUE
) {
  check_character(allowed_domains, allow_null = TRUE)
  check_bool(external_web_access)

  # Strip http/https from domains
  if (!is.null(allowed_domains)) {
    allowed_domains <- sub("^https?://", "", allowed_domains)
  }

  json <- compact(list(
    type = "web_search",
    filters = if (!is.null(allowed_domains)) {
      list(allowed_domains = allowed_domains)
    },
    user_location = user_location,
    external_web_access = external_web_access
  ))
  ToolBuiltIn("openai_web_search", json)
}
