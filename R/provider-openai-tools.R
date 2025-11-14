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
#' chat <- chat_openai()
#' chat$register_tool(openai_tool_web_search())
#' chat$chat("Very briefly summarise the top 3 news stories of the day")
#' chat$chat("Of those stories, which one do you think was the most interesting?")
#' }
openai_tool_web_search <- function(
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
  ToolBuiltIn("web_search", json)
}
