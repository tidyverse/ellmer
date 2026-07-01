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
#>    provider             model input output cached_input price
#> 1    OpenAI           gpt-5.4   818    574            0 $0.01
#> 2 Anthropic claude-sonnet-4-6    14    146            0 $0.00
```
