# Tool annotations

Tool annotations are additional properties that, when passed to the
`.annotations` argument of
[`tool()`](https://ellmer.tidyverse.org/dev/reference/tool.md), provide
additional information about the tool and its behavior. This information
can be used for display to users, for example in a Shiny app or another
user interface.

The annotations in `tool_annotations()` are drawn from the [Model
Context Protocol](https://modelcontextprotocol.io/introduction) and are
considered *hints*. Tool authors should use these annotations to
communicate tool properties, but users should note that these
annotations are not guaranteed.

## Usage

``` r
tool_annotations(
  title = NULL,
  read_only_hint = NULL,
  open_world_hint = NULL,
  idempotent_hint = NULL,
  destructive_hint = NULL,
  ...
)
```

## Arguments

- title:

  A human-readable title for the tool.

- read_only_hint:

  If `TRUE`, the tool does not modify its environment.

- open_world_hint:

  If `TRUE`, the tool may interact with an "open world" of external
  entities. If `FALSE`, the tool's domain of interaction is closed. For
  example, the world of a web search tool is open, but the world of a
  memory tool is not.

- idempotent_hint:

  If `TRUE`, calling the tool repeatedly with the same arguments will
  have no additional effect on its environment. (Only meaningful when
  `read_only_hint` is `FALSE`.)

- destructive_hint:

  If `TRUE`, the tool may perform destructive updates to its
  environment, otherwise it only performs additive updates. (Only
  meaningful when `read_only_hint` is `FALSE`.)

- ...:

  Additional named parameters to include in the tool annotations.

## Value

A list of tool annotations.

## See also

Other tool calling helpers:
[`tool()`](https://ellmer.tidyverse.org/dev/reference/tool.md),
[`tool_reject()`](https://ellmer.tidyverse.org/dev/reference/tool_reject.md)

## Examples

``` r
# See ?tool() for a full example using this function.
# We're creating a tool around R's `rnorm()` function to allow the chatbot to
# generate random numbers from a normal distribution.
tool_rnorm <- tool(
  rnorm,
  # Describe the tool function to the LLM
  .description = "Drawn numbers from a random normal distribution",
  # Describe the parameters used by the tool function
  n = type_integer("The number of observations. Must be a positive integer."),
  mean = type_number("The mean value of the distribution."),
  sd = type_number("The standard deviation of the distribution. Must be a non-negative number."),
  # Tool annotations optionally provide additional context to the LLM
  .annotations = tool_annotations(
    title = "Draw Random Normal Numbers",
    read_only_hint = TRUE, # the tool does not modify any state
    open_world_hint = FALSE # the tool does not interact with the outside world
  )
)
#> Warning: The `...` argument of `tool()` is deprecated as of ellmer 0.3.0.
#> ℹ Please use the `arguments` argument instead.
#> Warning: The `.description` argument of `tool()` is deprecated as of ellmer
#> 0.3.0.
#> ℹ Please use the `description` argument instead.
#> Warning: The `.annotations` argument of `tool()` is deprecated as of ellmer
#> 0.3.0.
#> ℹ Please use the `annotations` argument instead.
```
