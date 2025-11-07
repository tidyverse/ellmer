# Create metadata for a tool

In order to use a function as a tool in a chat, you need to craft the
right call to
[`tool()`](https://ellmer.tidyverse.org/dev/reference/tool.md). This
function helps you do that for documented functions by extracting the
function's R documentation and using an LLM to generate the
[`tool()`](https://ellmer.tidyverse.org/dev/reference/tool.md) call.
It's meant to be used interactively while writing your code, not as part
of your final code.

If the function has package documentation, that will be used. Otherwise,
if the source code of the function can be automatically detected, then
the comments immediately preceding the function are used (especially
helpful if those are roxygen2 comments). If neither are available, then
just the function signature is used.

Note that this function is inherently imperfect. It can't handle all
possible R functions, because not all parameters are suitable for use in
a tool call (for example, because they're not serializable to simple
JSON objects). The documentation might not specify the expected shape of
arguments to the level of detail that would allow an exact JSON schema
to be generated. Please be sure to review the generated code before
using it!

## Usage

``` r
create_tool_def(topic, chat = NULL, echo = interactive(), verbose = FALSE)
```

## Arguments

- topic:

  A symbol or string literal naming the function to create metadata for.
  Can also be an expression of the form `pkg::fun`.

- chat:

  A `Chat` object used to generate the output. If `NULL` (the default)
  uses
  [`chat_openai()`](https://ellmer.tidyverse.org/dev/reference/chat_openai.md).

- echo:

  Emit the registration code to the console. Defaults to `TRUE` in
  interactive sessions.

- verbose:

  If `TRUE`, print the input we send to the LLM, which may be useful for
  debugging unexpectedly poor results.

## Value

A `register_tool` call that you can copy and paste into your code.
Returned invisibly if `echo` is `TRUE`.

## Examples

``` r
if (FALSE) { # \dontrun{
  # These are all equivalent
  create_tool_def(rnorm)
  create_tool_def(stats::rnorm)
  create_tool_def("rnorm")
  create_tool_def("rnorm", chat = chat_azure_openai())
} # }
```
