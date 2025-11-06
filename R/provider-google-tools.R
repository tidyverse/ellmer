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
