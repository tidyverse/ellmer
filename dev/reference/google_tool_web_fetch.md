# Google URL fetch tool

When this tool is enabled, you can include URLs directly in your prompts
and Gemini will fetch and analyze the content.

Learn more in <https://ai.google.dev/gemini-api/docs/url-context>.

## Usage

``` r
google_tool_web_fetch()
```

## See also

Other built-in tools:
[`claude_tool_web_fetch()`](https://ellmer.tidyverse.org/dev/reference/claude_tool_web_fetch.md),
[`claude_tool_web_search()`](https://ellmer.tidyverse.org/dev/reference/claude_tool_web_search.md),
[`google_tool_web_search()`](https://ellmer.tidyverse.org/dev/reference/google_tool_web_search.md),
[`openai_tool_web_search()`](https://ellmer.tidyverse.org/dev/reference/openai_tool_web_search.md)

## Examples

``` r
if (FALSE) { # \dontrun{
chat <- chat_google_gemini()
chat$register_tool(google_tool_web_fetch())
chat$chat("What are the latest package releases on https://tidyverse.org/blog?")
} # }
```
