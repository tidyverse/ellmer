# Define a tool

Annotate a function for use in tool calls, by providing a name,
description, and type definition for the arguments.

Learn more in
[`vignette("tool-calling")`](https://ellmer.tidyverse.org/dev/articles/tool-calling.md).

## Usage

``` r
tool(
  fun,
  description,
  ...,
  arguments = list(),
  name = NULL,
  convert = TRUE,
  annotations = list(),
  .name = deprecated(),
  .description = deprecated(),
  .convert = deprecated(),
  .annotations = deprecated()
)
```

## Arguments

- fun:

  The function to be invoked when the tool is called. The return value
  of the function is sent back to the chatbot.

  Expert users can customize the tool result by returning a
  [ContentToolResult](https://ellmer.tidyverse.org/dev/reference/Content.md)
  object.

- description:

  A detailed description of what the function does. Generally, the more
  information that you can provide here, the better.

- ...:

  **\[deprecated\]** Use `arguments` instead.

- arguments:

  A named list that defines the arguments accepted by the function. Each
  element should be created by a
  [`type_*()`](https://ellmer.tidyverse.org/dev/reference/type_boolean.md)
  function. Use
  [`type_ignore()`](https://ellmer.tidyverse.org/dev/reference/type_boolean.md)
  if you don't want the LLM to provide that argument (e.g., because the
  R function has a suitable default value).

- name:

  The name of the function. This can be omitted if `fun` is an existing
  function (i.e. not defined inline).

- convert:

  Should JSON inputs be automatically convert to their R data type
  equivalents? Defaults to `TRUE`.

- annotations:

  Additional properties that describe the tool and its behavior. Usually
  created by
  [`tool_annotations()`](https://ellmer.tidyverse.org/dev/reference/tool_annotations.md),
  where you can find a description of the annotation properties
  recommended by the [Model Context
  Protocol](https://modelcontextprotocol.io/introduction).

- .name, .description, .convert, .annotations:

  **\[deprecated\]** Please switch to the non-prefixed equivalents.

## Value

An S7 `ToolDef` object.

## ellmer 0.3.0

In ellmer 0.3.0, the definition of the `tool()` function changed quite a
bit. To make it easier to update old versions, you can use an LLM with
the following system prompt

    Help the user convert an ellmer 0.2.0 and earlier tool definition into a
    ellmer 0.3.0 tool definition. Here's what changed:

    * All arguments, apart from the first, should be named, and the argument
      names no longer use `.` prefixes. The argument order should be function,
      name (as a string), description, then arguments, then anything

    * Previously `arguments` was passed as `...`, so all type specifications
      should now be moved into a named list and passed to the `arguments`
      argument. It can be omitted if the function has no arguments.

    ```R
    # old
    tool(
      add,
      "Add two numbers together"
      x = type_number(),
      y = type_number()
    )

    # new
    tool(
      add,
      name = "add",
      description = "Add two numbers together",
      arguments = list(
        x = type_number(),
        y = type_number()
      )
    )
    ```

    Don't respond; just let the user provide function calls to convert.

## See also

Other tool calling helpers:
[`tool_annotations()`](https://ellmer.tidyverse.org/dev/reference/tool_annotations.md),
[`tool_reject()`](https://ellmer.tidyverse.org/dev/reference/tool_reject.md)

## Examples

``` r
# First define the metadata that the model uses to figure out when to
# call the tool
tool_rnorm <- tool(
  rnorm,
  description = "Draw numbers from a random normal distribution",
  arguments = list(
    n = type_integer("The number of observations. Must be a positive integer."),
    mean = type_number("The mean value of the distribution."),
    sd = type_number("The standard deviation of the distribution. Must be a non-negative number.")
  )
)
tool_rnorm(n = 5, mean = 0, sd = 1)
#> [1] -1.400043517  0.255317055 -2.437263611 -0.005571287  0.621552721

chat <- chat_openai()
#> Using model = "gpt-4.1".
# Then register it
chat$register_tool(tool_rnorm)

# Then ask a question that needs it.
chat$chat("Give me five numbers from a random normal distribution.")
#> Here are five numbers drawn from a random normal distribution (mean = 
#> 0, standard deviation = 1):
#> 
#> 1. 1.1484
#> 2. -1.8218
#> 3. -0.2473
#> 4. -0.2442
#> 5. -0.2827

# Look at the chat history to see how tool calling works:
chat
#> <Chat OpenAI/gpt-4.1 turns=4 input=234 output=86 cost=$0.00>
#> ── user ───────────────────────────────────────────────────────────────
#> Give me five numbers from a random normal distribution.
#> ── assistant [input=90 output=23 cost=$0.00] ──────────────────────────
#> [tool request (fc_0b0635de4f44748f01692dba5a16388193a8de793c57908c2f)]: rnorm(n = 5L, mean = 0L, sd = 1L)
#> ── user ───────────────────────────────────────────────────────────────
#> [tool result  (fc_0b0635de4f44748f01692dba5a16388193a8de793c57908c2f)]: [1.1484,-1.8218,-0.2473,-0.2442,-0.2827]
#> ── assistant [input=144 output=63 cost=$0.00] ─────────────────────────
#> Here are five numbers drawn from a random normal distribution (mean = 0, standard deviation = 1):
#> 
#> 1. 1.1484
#> 2. -1.8218
#> 3. -0.2473
#> 4. -0.2442
#> 5. -0.2827
# Assistant sends a tool request which is evaluated locally and
# results are sent back in a tool result.
```
