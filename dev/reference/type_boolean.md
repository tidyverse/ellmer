# Type specifications

These functions specify object types in a way that chatbots understand
and are used for tool calling and structured data extraction. Their
names are based on the [JSON schema](https://json-schema.org), which is
what the APIs expect behind the scenes. The translation from R concepts
to these types is fairly straightforward.

- `type_boolean()`, `type_integer()`, `type_number()`, and
  `type_string()` each represent scalars. These are equivalent to
  length-1 logical, integer, double, and character vectors
  (respectively).

- `type_enum()` is equivalent to a length-1 factor; it is a string that
  can only take the specified values.

- `type_array()` is equivalent to a vector in R. You can use it to
  represent an atomic vector: e.g. `type_array(type_boolean())` is
  equivalent to a logical vector and `type_array(type_string())` is
  equivalent to a character vector). You can also use it to represent a
  list of more complicated types where every element is the same type (R
  has no base equivalent to this), e.g.
  `type_array(type_array(type_string()))` represents a list of character
  vectors.

- `type_object()` is equivalent to a named list in R, but where every
  element must have the specified type. For example,
  `type_object(a = type_string(), b = type_array(type_integer()))` is
  equivalent to a list with an element called `a` that is a string and
  an element called `b` that is an integer vector.

- `type_ignore()` is used in tool calling to indicate that an argument
  should not be provided by the LLM. This is useful when the R function
  has a default value for the argument and you don't want the LLM to
  supply it.

- `type_from_schema()` allows you to specify the full schema that you
  want to get back from the LLM as a JSON schema. This is useful if you
  have a pre-defined schema that you want to use directly without
  manually creating the type using the `type_*()` functions. You can
  point to a file with the `path` argument or provide a JSON string with
  `text`. The schema must be a valid JSON schema object.

## Usage

``` r
type_boolean(description = NULL, required = TRUE)

type_integer(description = NULL, required = TRUE)

type_number(description = NULL, required = TRUE)

type_string(description = NULL, required = TRUE)

type_enum(values, description = NULL, required = TRUE)

type_array(items, description = NULL, required = TRUE)

type_object(
  .description = NULL,
  ...,
  .required = TRUE,
  .additional_properties = FALSE
)

type_from_schema(text, path)

type_ignore()
```

## Arguments

- description, .description:

  The purpose of the component. This is used by the LLM to determine
  what values to pass to the tool or what values to extract in the
  structured data, so the more detail that you can provide here, the
  better.

- required, .required:

  Is the component or argument required?

  In type descriptions for structured data, if `required = FALSE` and
  the component does not exist in the data, the LLM may hallucinate a
  value. Only applies when the element is nested inside of a
  `type_object()`.

  In tool definitions, `required = TRUE` signals that the LLM should
  always provide a value. Arguments with `required = FALSE` should have
  a default value in the tool function's definition. If the LLM does not
  provide a value, the default value will be used.

- values:

  Character vector of permitted values.

- items:

  The type of the array items. Can be created by any of the `type_`
  function.

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  Name-type pairs defining the components that the object must possess.

- .additional_properties:

  Can the object have arbitrary additional properties that are not
  explicitly listed? Only supported by Claude.

- text:

  A JSON string.

- path:

  A file path to a JSON file.

## Examples

``` r
# An integer vector
type_array(type_integer())
#> <ellmer::TypeArray>
#>  @ description: NULL
#>  @ required   : logi TRUE
#>  @ items      : <ellmer::TypeBasic>
#>  .. @ description: NULL
#>  .. @ required   : logi TRUE
#>  .. @ type       : chr "integer"

# The closest equivalent to a data frame is an array of objects
type_array(type_object(
   x = type_boolean(),
   y = type_string(),
   z = type_number()
))
#> <ellmer::TypeArray>
#>  @ description: NULL
#>  @ required   : logi TRUE
#>  @ items      : <ellmer::TypeObject>
#>  .. @ description          : NULL
#>  .. @ required             : logi TRUE
#>  .. @ properties           :List of 3
#>  .. .. $ x: <ellmer::TypeBasic>
#>  .. ..  ..@ description: NULL
#>  .. ..  ..@ required   : logi TRUE
#>  .. ..  ..@ type       : chr "boolean"
#>  .. .. $ y: <ellmer::TypeBasic>
#>  .. ..  ..@ description: NULL
#>  .. ..  ..@ required   : logi TRUE
#>  .. ..  ..@ type       : chr "string"
#>  .. .. $ z: <ellmer::TypeBasic>
#>  .. ..  ..@ description: NULL
#>  .. ..  ..@ required   : logi TRUE
#>  .. ..  ..@ type       : chr "number"
#>  .. @ additional_properties: logi FALSE

# There's no specific type for dates, but you use a string with the
# requested format in the description (it's not gauranteed that you'll
# get this format back, but you should most of the time)
type_string("The creation date, in YYYY-MM-DD format.")
#> <ellmer::TypeBasic>
#>  @ description: chr "The creation date, in YYYY-MM-DD format."
#>  @ required   : logi TRUE
#>  @ type       : chr "string"
type_string("The update date, in dd/mm/yyyy format.")
#> <ellmer::TypeBasic>
#>  @ description: chr "The update date, in dd/mm/yyyy format."
#>  @ required   : logi TRUE
#>  @ type       : chr "string"
```
