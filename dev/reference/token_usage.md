# Report on token usage in the current session

Call this function to find out the cumulative number of tokens that you
have sent and received in the current session. The price will be shown
if known.

## Usage

``` r
token_usage()
```

## Value

A data frame

## Details

Most providers' costs are estimated from published token rates.
OpenRouter's cost reporting differs; see
[`chat_openrouter()`](https://ellmer.tidyverse.org/dev/reference/chat_openrouter.md)
for details.

An unknown charge makes the cumulative total for that provider and model
unknown, while known individual turn costs remain available through the
chat object's `get_tokens()` method.

## Examples

``` r
token_usage()
#>    provider             model input output cached_input price
#> 1    OpenAI     gpt-6.1-terra   873    669            0 $0.00
#> 2 Anthropic claude-sonnet-5-5    21    283            0 $0.00
```
