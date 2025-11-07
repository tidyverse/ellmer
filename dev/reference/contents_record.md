# Record and replay content

These generic functions can be use to convert
[Turn](https://ellmer.tidyverse.org/dev/reference/Turn.md)/[Content](https://ellmer.tidyverse.org/dev/reference/Content.md)
objects into easily serializable representations (i.e. lists and atomic
vectors).

- `contents_record()` accepts a
  [Turn](https://ellmer.tidyverse.org/dev/reference/Turn.md) or
  [Content](https://ellmer.tidyverse.org/dev/reference/Content.md) and
  return a simple list.

- `contents_replay()` takes the output of `contents_record()` and
  returns a [Turn](https://ellmer.tidyverse.org/dev/reference/Turn.md)
  or [Content](https://ellmer.tidyverse.org/dev/reference/Content.md)
  object.

## Usage

``` r
contents_record(x)

contents_replay(x, tools = list(), .envir = parent.frame())
```

## Arguments

- x:

  A [Turn](https://ellmer.tidyverse.org/dev/reference/Turn.md) or
  [Content](https://ellmer.tidyverse.org/dev/reference/Content.md)
  object to serialize; or a serialized object to replay.

- tools:

  A named list of tools

- .envir:

  The environment in which to look for class definitions. Used when the
  recorded objects include classes that extend
  [Turn](https://ellmer.tidyverse.org/dev/reference/Turn.md) or
  [Content](https://ellmer.tidyverse.org/dev/reference/Content.md) but
  are not from the ellmer package itself.
