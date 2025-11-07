# Format contents into a textual representation

**\[experimental\]**

These generic functions can be use to convert
[Turn](https://ellmer.tidyverse.org/dev/reference/Turn.md) contents or
[Content](https://ellmer.tidyverse.org/dev/reference/Content.md) objects
into textual representations.

- `contents_text()` is the most minimal and only includes
  [ContentText](https://ellmer.tidyverse.org/dev/reference/Content.md)
  objects in the output.

- `contents_markdown()` returns the text content (which it assumes to be
  markdown and does not convert it) plus markdown representations of
  images and other content types.

- `contents_html()` returns the text content, converted from markdown to
  HTML with
  [`commonmark::markdown_html()`](https://docs.ropensci.org/commonmark/reference/commonmark.html),
  plus HTML representations of images and other content types.

These content types will continue to grow and change as ellmer evolves
to support more providers and as providers add more content types.

## Usage

``` r
contents_text(content, ...)

contents_html(content, ...)

contents_markdown(content, ...)
```

## Arguments

- content:

  The [Turn](https://ellmer.tidyverse.org/dev/reference/Turn.md) or
  [Content](https://ellmer.tidyverse.org/dev/reference/Content.md)
  object to be converted into text. `contents_markdown()` also accepts
  [Chat](https://ellmer.tidyverse.org/dev/reference/Chat.md) instances
  to turn the entire conversation history into markdown text.

- ...:

  Additional arguments passed to methods.

## Value

A string of text, markdown or HTML.

## Examples

``` r
turns <- list(
  UserTurn(list(
    ContentText("What's this image?"),
    content_image_url("https://placehold.co/200x200")
  )),
  AssistantTurn("It's a placeholder image.")
)

lapply(turns, contents_text)
#> [[1]]
#> [1] "What's this image?"
#> 
#> [[2]]
#> [1] "It's a placeholder image."
#> 
lapply(turns, contents_markdown)
#> [[1]]
#> [1] "What's this image?\n\n![](https://placehold.co/200x200)"
#> 
#> [[2]]
#> [1] "It's a placeholder image."
#> 
if (rlang::is_installed("commonmark")) {
  contents_html(turns[[1]])
}
#> [1] "<p>What's this image?</p>\n\n<img src=\"https://placehold.co/200x200\">"
```
