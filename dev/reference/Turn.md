# A user, assistant, or system turn

Every conversation with a chatbot consists of pairs of user and
assistant turns, corresponding to an HTTP request and response. These
turns are represented by the `Turn` object, which contains a list of
[Content](https://ellmer.tidyverse.org/dev/reference/Content.md)s
representing the individual messages within the turn. These might be
text, images, tool requests (assistant only), or tool responses (user
only).

`UserTurn`, `AssistantTurn`, and `SystemTurn` are specialized subclasses
of `Turn` for different types of conversation turns. `AssistantTurn`
includes additional metadata about the API response.

Note that a call to `$chat()` and related functions may result in
multiple user-assistant turn cycles. For example, if you have registered
tools, ellmer will automatically handle the tool calling loop, which may
result in any number of additional cycles. Learn more about tool calling
in
[`vignette("tool-calling")`](https://ellmer.tidyverse.org/dev/articles/tool-calling.md).

## Usage

``` r
Turn(role = NULL, contents = list(), tokens = NULL)

UserTurn(contents = list())

SystemTurn(contents = list())

AssistantTurn(
  contents = list(),
  json = list(),
  tokens = c(NA_real_, NA_real_, NA_real_),
  cost = NA_real_,
  duration = NA_real_
)
```

## Arguments

- role:

  **\[deprecated\]** For system, user and assistant turns, use
  `SystemTurn()`, `UserTurn()`, and `AssistantTurn()`, respectively.

- contents:

  A list of
  [Content](https://ellmer.tidyverse.org/dev/reference/Content.md)
  objects.

- tokens:

  A numeric vector of length 3 representing the number of input tokens
  (uncached), output tokens, and input tokens (cached) used in this
  turn.

- json:

  The serialized JSON corresponding to the underlying data of the turns.
  This is useful if there's information returned by the provider that
  ellmer doesn't otherwise expose.

- cost:

  The cost of the turn in dollars.

- duration:

  The duration of the request in seconds.

## Value

An S7 `Turn` object

An S7 `AssistantTurn` object

## Examples

``` r
UserTurn(list(ContentText("Hello, world!")))
#> <Turn: user>
#> Hello, world!
```
