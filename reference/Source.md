# Sources referenced by model content

**\[experimental\]**

`Source` is the base class for evidence referenced by model-generated
content. `WebSource` identifies a web page surfaced by search or
citation metadata. Providers do not always supply both a URL and title,
so either field may be `NULL`.

## Usage

``` r
Source()

WebSource(url = NULL, title = NULL)
```

## Arguments

- url:

  The URL of the web page, or `NULL` when unavailable.

- title:

  The title of the web page, or `NULL` when unavailable.

## Value

An S7 object that inherits from `Source`.

## Examples

``` r
WebSource("https://example.com", "Example")
#> <ellmer::WebSource>
#>  @ url  : chr "https://example.com"
#>  @ title: chr "Example"
WebSource(title = "Source without a URL")
#> <ellmer::WebSource>
#>  @ url  : NULL
#>  @ title: chr "Source without a URL"
```
