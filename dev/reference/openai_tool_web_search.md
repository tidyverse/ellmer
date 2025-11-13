# OpenAI web search tool

Enables OpenAI models to search the web for up-to-date information. The
search behavior varies by model: non-reasoning models perform simple
searches, while reasoning models can perform agentic, iterative
searches.

Learn more at <https://platform.openai.com/docs/guides/tools-web-search>

## Usage

``` r
openai_tool_web_search(
  allowed_domains = NULL,
  user_location = NULL,
  external_web_access = TRUE
)
```

## Arguments

- allowed_domains:

  Character vector. Restrict searches to specific domains (e.g.,
  `c("nytimes.com", "bbc.com")`). Maximum 20 domains. URLs will be
  automatically cleaned (http/https prefixes removed).

- user_location:

  List with optional elements: `country` (2-letter ISO code), `city`,
  `region`, and `timezone` (IANA timezone) to localize search results.

- external_web_access:

  Logical. Whether to allow live internet access (`TRUE`, default) or
  use only cached/indexed results (`FALSE`).

## See also

Other built-in tools:
[`claude_tool_web_fetch()`](https://ellmer.tidyverse.org/dev/reference/claude_tool_web_fetch.md),
[`claude_tool_web_search()`](https://ellmer.tidyverse.org/dev/reference/claude_tool_web_search.md),
[`google_tool_web_fetch()`](https://ellmer.tidyverse.org/dev/reference/google_tool_web_fetch.md),
[`google_tool_web_search()`](https://ellmer.tidyverse.org/dev/reference/google_tool_web_search.md)

## Examples

``` r
if (FALSE) { # \dontrun{
chat <- chat_openai()
chat$register_tool(openai_tool_web_search())
chat$chat("Very briefly summarise the top 3 news stories of the day")
chat$chat("Of those stories, which one do you think was the most interesting?")
} # }
```
