# Report on token usage in the current session

Call this function to find out the cumulative number of tokens that you
have sent and recieved in the current session. The price will be shown
if known.

## Usage

``` r
token_usage()
```

## Value

A data frame

## Examples

``` r
token_usage()
#>    provider                      model input output cached_input price
#> 1    OpenAI                    gpt-4.1   908    662            0 $0.01
#> 2 Anthropic claude-sonnet-4-5-20250929    14    215            0 $0.00
```
