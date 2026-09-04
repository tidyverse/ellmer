# A tool definition

An S7 class representing a tool that can be called by a chat model. You
should generally not create this object yourself, but instead call
[`tool()`](https://ellmer.tidyverse.org/reference/tool.md) instead.

## Usage

``` r
ToolDef(
  .data = function() NULL,
  name = stop("Required"),
  description = stop("Required"),
  arguments = TypeObject(),
  convert = TRUE,
  annotations = list()
)
```

## Arguments

- .data:

  The underlying function.

- name:

  The name of the tool.

- description:

  A description of what the tool does.

- arguments:

  A [TypeObject](https://ellmer.tidyverse.org/reference/Type.md)
  describing the tool's arguments.

- convert:

  Whether to automatically convert JSON inputs to R equivalents.

- annotations:

  A list of additional tool annotations.

## Examples

``` r
my_tool <- ToolDef(
  function(x) x * 2,
  name = "double",
  description = "Doubles a number",
  arguments = type_object(x = type_number("The number to double"))
)
```
