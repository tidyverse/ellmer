# A model configuration

A `Model` captures the details of a specific model: its name, standard
parameters, and any extra arguments to include in the API request body.
This is paired with a
[Provider](https://ellmer.tidyverse.org/reference/Provider.md), which
captures *who* you're talking to, while the `Model` captures *what*
you're asking for.

## Usage

``` r
Model(name = stop("Required"), params = list(), extra_args = list())
```

## Arguments

- name:

  Name of the model (e.g. `"gpt-4.1"`, `"claude-sonnet-4-6"`).

- params:

  A list of standard parameters created by
  [`params()`](https://ellmer.tidyverse.org/reference/params.md).

- extra_args:

  Arbitrary extra arguments to be included in the request body.

## Value

An S7 Model object.

## Details

You generally don't need to create `Model` objects directly; they are
created automatically by `chat_*()` functions like
[`chat_openai()`](https://ellmer.tidyverse.org/reference/chat_openai.md)
and
[`chat_anthropic()`](https://ellmer.tidyverse.org/reference/chat_anthropic.md).

## Examples

``` r
Model(name = "gpt-4.1")
#> <ellmer::Model>
#>  @ name      : chr "gpt-4.1"
#>  @ params    : list()
#>  @ extra_args: list()
Model(name = "claude-sonnet-4-6", params = params(temperature = 0))
#> <ellmer::Model>
#>  @ name      : chr "claude-sonnet-4-6"
#>  @ params    :List of 1
#>  .. $ temperature: num 0
#>  @ extra_args: list()
```
