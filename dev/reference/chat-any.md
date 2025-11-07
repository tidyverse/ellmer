# Chat with any provider

This is a generic interface to all the other `chat_` functions that
allow to you pick the provider and the model with a simple string.

## Usage

``` r
chat(
  name,
  ...,
  system_prompt = NULL,
  params = NULL,
  echo = c("none", "output", "all")
)
```

## Arguments

- name:

  Provider (and optionally model) name in the form `"provider/model"` or
  `"provider"` (which will use the default model for that provider).

- ...:

  Arguments passed to the provider function.

- system_prompt:

  A system prompt to set the behavior of the assistant.

- params:

  Common model parameters, usually created by
  [`params()`](https://ellmer.tidyverse.org/dev/reference/params.md).

- echo:

  One of the following options:

  - `none`: don't emit any output (default when running in a function).

  - `output`: echo text and tool-calling output as it streams in
    (default when running at the console).

  - `all`: echo all input and output.

  Note this only affects the `chat()` method.
