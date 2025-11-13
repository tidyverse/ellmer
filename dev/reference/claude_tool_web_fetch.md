# Claude web fetch tool

Enables Claude to fetch and analyze content from web URLs. Claude can
only fetch URLs that appear in the conversation context (user messages
or previous tool results). For security reasons, Claude cannot
dynamically construct URLs to fetch.

Requires the `web-fetch-2025-09-10` beta header. Learn more in
<https://docs.claude.com/en/docs/agents-and-tools/tool-use/web-fetch-tool>.

## Usage

``` r
claude_tool_web_fetch(
  max_uses = NULL,
  allowed_domains = NULL,
  blocked_domains = NULL,
  citations = FALSE,
  max_content_tokens = NULL
)
```

## Arguments

- max_uses:

  Integer. Maximum number of fetches allowed per request.

- allowed_domains:

  Character vector. Restrict fetches to specific domains. Cannot be used
  with `blocked_domains`.

- blocked_domains:

  Character vector. Exclude specific domains from fetches. Cannot be
  used with `allowed_domains`.

- citations:

  Logical. Whether to include citations in the response. Default is
  `TRUE`.

- max_content_tokens:

  Integer. Maximum number of tokens to fetch from each URL.

## See also

Other built-in tools:
[`claude_tool_web_search()`](https://ellmer.tidyverse.org/dev/reference/claude_tool_web_search.md),
[`google_tool_web_fetch()`](https://ellmer.tidyverse.org/dev/reference/google_tool_web_fetch.md),
[`google_tool_web_search()`](https://ellmer.tidyverse.org/dev/reference/google_tool_web_search.md),
[`openai_tool_web_search()`](https://ellmer.tidyverse.org/dev/reference/openai_tool_web_search.md)

## Examples

``` r
if (FALSE) { # \dontrun{
chat <- chat_claude(beta_headers = "web-fetch-2025-09-10")
chat$register_tool(claude_tool_web_fetch())
chat$chat("What are the latest package releases on https://tidyverse.org/blog")
} # }
```
