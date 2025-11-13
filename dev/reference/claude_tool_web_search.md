# Claude web search tool

Enables Claude to search the web for up-to-date information. Your
organization administrator must enable web search in the Anthropic
Console before using this tool, as it costs extra (\$10 per 1,000 tokens
at time of writing).

Learn more in
<https://docs.claude.com/en/docs/agents-and-tools/tool-use/web-search-tool>.

## Usage

``` r
claude_tool_web_search(
  max_uses = NULL,
  allowed_domains = NULL,
  blocked_domains = NULL,
  user_location = NULL
)
```

## Arguments

- max_uses:

  Integer. Maximum number of searches allowed per request.

- allowed_domains:

  Character vector. Restrict searches to specific domains (e.g.,
  `c("nytimes.com", "bbc.com")`). Cannot be used with `blocked_domains`.

- blocked_domains:

  Character vector. Exclude specific domains from searches. Cannot be
  used with `allowed_domains`.

- user_location:

  List with optional elements: `country` (2-letter code), `city`,
  `region`, and `timezone` (IANA timezone) to localize search results.

## See also

Other built-in tools:
[`claude_tool_web_fetch()`](https://ellmer.tidyverse.org/dev/reference/claude_tool_web_fetch.md),
[`google_tool_web_fetch()`](https://ellmer.tidyverse.org/dev/reference/google_tool_web_fetch.md),
[`google_tool_web_search()`](https://ellmer.tidyverse.org/dev/reference/google_tool_web_search.md),
[`openai_tool_web_search()`](https://ellmer.tidyverse.org/dev/reference/openai_tool_web_search.md)

## Examples

``` r
if (FALSE) { # \dontrun{
chat <- chat_claude()
chat$register_tool(claude_tool_web_search())
chat$chat("What was in the news today?")
chat$chat("What's the biggest news in the economy?")
} # }
```
