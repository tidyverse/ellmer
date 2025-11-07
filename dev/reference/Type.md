# Type definitions for function calling and structured data extraction.

These S7 classes are provided for use by package devlopers who are
extending ellmer. In every day use, use
[`type_boolean()`](https://ellmer.tidyverse.org/dev/reference/type_boolean.md)
and friends.

## Usage

``` r
TypeBasic(description = NULL, required = TRUE, type = stop("Required"))

TypeEnum(description = NULL, required = TRUE, values = character(0))

TypeArray(description = NULL, required = TRUE, items = Type())

TypeJsonSchema(description = NULL, required = TRUE, json = list())

TypeIgnore(description = NULL, required = TRUE)

TypeObject(
  description = NULL,
  required = TRUE,
  properties = list(),
  additional_properties = FALSE
)
```

## Arguments

- description:

  The purpose of the component. This is used by the LLM to determine
  what values to pass to the tool or what values to extract in the
  structured data, so the more detail that you can provide here, the
  better.

- required:

  Is the component or argument required?

  In type descriptions for structured data, if `required = FALSE` and
  the component does not exist in the data, the LLM may hallucinate a
  value. Only applies when the element is nested inside of a
  [`type_object()`](https://ellmer.tidyverse.org/dev/reference/type_boolean.md).

  In tool definitions, `required = TRUE` signals that the LLM should
  always provide a value. Arguments with `required = FALSE` should have
  a default value in the tool function's definition. If the LLM does not
  provide a value, the default value will be used.

- type:

  Basic type name. Must be one of `boolean`, `integer`, `number`, or
  `string`.

- values:

  Character vector of permitted values.

- items:

  The type of the array items. Can be created by any of the `type_`
  function.

- json:

  A JSON schema object as a list.

- properties:

  Named list of properties stored inside the object. Each element should
  be an S7 `Type` object.\`

- additional_properties:

  Can the object have arbitrary additional properties that are not
  explicitly listed? Only supported by Claude.

## Value

S7 objects inheriting from `Type`

## Examples

``` r
TypeBasic(type = "boolean")
#> <ellmer::TypeBasic>
#>  @ description: NULL
#>  @ required   : logi TRUE
#>  @ type       : chr "boolean"
TypeArray(items = TypeBasic(type = "boolean"))
#> <ellmer::TypeArray>
#>  @ description: NULL
#>  @ required   : logi TRUE
#>  @ items      : <ellmer::TypeBasic>
#>  .. @ description: NULL
#>  .. @ required   : logi TRUE
#>  .. @ type       : chr "boolean"
```
