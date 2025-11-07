# Open a live chat application

- `live_console()` lets you chat interactively in the console.

- `live_browser()` lets you chat interactively in a browser.

Note that these functions will mutate the input `chat` object as you
chat because your turns will be appended to the history.

## Usage

``` r
live_console(chat, quiet = FALSE)

live_browser(chat, quiet = FALSE)
```

## Arguments

- chat:

  A chat object created by
  [`chat_openai()`](https://ellmer.tidyverse.org/dev/reference/chat_openai.md)
  or friends.

- quiet:

  If `TRUE`, suppresses the initial message that explains how to use the
  console.

## Value

(Invisibly) The input `chat`.

## Examples

``` r
if (FALSE) { # \dontrun{
chat <- chat_anthropic()
live_console(chat)
live_browser(chat)
} # }
```
