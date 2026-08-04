# A round of conversation

A `Round` groups a user
[Turn](https://ellmer.tidyverse.org/dev/reference/Turn.md) with the
assistant and tool-result
[Turn](https://ellmer.tidyverse.org/dev/reference/Turn.md)s that follow
it, i.e. everything that happens in response to one user message,
including any tool-calling loop. `Round`s are an alternative view of a
[Chat](https://ellmer.tidyverse.org/dev/reference/Chat.md)'s flat turn
history.

`Round`s also expose a read-only `@complete` property: `TRUE` if
`response` is non-empty and its last element is a finished (non-partial)
assistant turn with no pending tool request, `FALSE` otherwise.

## Usage

``` r
Round(input = list(), response = list())
```

## Arguments

- input:

  A list of the input-side
  [Turn](https://ellmer.tidyverse.org/dev/reference/Turn.md)s that begin
  the round: the turns between the end of the previous round and the
  user turn that triggered this one. In the common case this is a
  length-1 list holding just that user turn, but it can also be preceded
  by system turns.

  Because [Chat](https://ellmer.tidyverse.org/dev/reference/Chat.md)'s
  `$set_turns()` accepts an arbitrary list of turns, `input` may in
  principle hold multiple user turns or system turns in other positions;
  a round consisting solely of system turns is also possible (e.g. a
  chat that so far only has a system prompt).

- response:

  A list of [Turn](https://ellmer.tidyverse.org/dev/reference/Turn.md)s
  (assistant and tool-result) that followed `input`.

## Value

An S7 `Round` object
