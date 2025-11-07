# Helpers for interpolating data into prompts

These functions are lightweight wrappers around
[glue](https://glue.tidyverse.org/) that make it easier to interpolate
dynamic data into a static prompt:

- `interpolate()` works with a string.

- `interpolate_file()` works with a file.

- `interpolate_package()` works with a file in the `inst/prompts`
  directory of a package.

Compared to glue, dynamic values should be wrapped in `{{ }}`, making it
easier to include R code and JSON in your prompt.

## Usage

``` r
interpolate(prompt, ..., .envir = parent.frame())

interpolate_file(path, ..., .envir = parent.frame())

interpolate_package(package, path, ..., .envir = parent.frame())
```

## Arguments

- prompt:

  A prompt string. You should not generally expose this to the end user,
  since glue interpolation makes it easy to run arbitrary code.

- ...:

  Define additional temporary variables for substitution.

- .envir:

  Environment to evaluate `...` expressions in. Used when wrapping in
  another function. See
  [`vignette("wrappers", package = "glue")`](https://glue.tidyverse.org/articles/wrappers.html)
  for more details.

- path:

  A path to a prompt file (often a `.md`). In `interpolate_package()`,
  this path is relative to `inst/prompts`.

- package:

  Package name.

## Value

A {glue} string.

## Examples

``` r
joke <- "You're a cool dude who loves to make jokes. Tell me a joke about {{topic}}."

# You can supply valuese directly:
interpolate(joke, topic = "bananas")
#> [1] │ You're a cool dude who loves to make jokes. Tell me a joke about bananas.

# Or allow interpolate to find them in the current environment:
topic <- "applies"
interpolate(joke)
#> [1] │ You're a cool dude who loves to make jokes. Tell me a joke about applies.

```
