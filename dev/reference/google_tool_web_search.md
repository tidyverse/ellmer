# Google web search (grounding) tool

Enables Gemini models to search the web for up-to-date information and
ground responses with citations to sources. The model automatically
decides when (and how) to search the web based on your prompt. Search
results are incorporated into the response with grounding metadata
including source URLs and titles.

Learn more in <https://ai.google.dev/gemini-api/docs/google-search>.

## Usage

``` r
google_tool_web_search()
```

## See also

Other built-in tools:
[`claude_tool_web_fetch()`](https://ellmer.tidyverse.org/dev/reference/claude_tool_web_fetch.md),
[`claude_tool_web_search()`](https://ellmer.tidyverse.org/dev/reference/claude_tool_web_search.md),
[`google_tool_web_fetch()`](https://ellmer.tidyverse.org/dev/reference/google_tool_web_fetch.md),
[`openai_tool_web_search()`](https://ellmer.tidyverse.org/dev/reference/openai_tool_web_search.md)

## Examples

``` r
if (FALSE) { # \dontrun{
chat <- chat_google_gemini()
chat$register_tool(google_tool_web_search())
chat$chat("What was in the news today?")
chat$chat("What's the biggest news in the economy?")
} # }
```
