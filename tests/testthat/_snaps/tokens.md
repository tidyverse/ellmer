# useful message if no tokens

    Code
      token_usage()
    Message
      x No recorded usage in this session

# can retrieve and log tokens

    Code
      token_usage()
    Output
            provider model input output cached_input price
      1 testprovider  test     1      1            1 $0.00

# informative internal error if variant is missing

    Code
      get_token_cost(provider, tokens(), variant = NULL)
    Condition
      Error in `get_token_cost()`:
      ! `variant` must be a single string, not `NULL`.
      i This is an internal error that was detected in the ellmer package.
        Please report it at <https://github.com/tidyverse/ellmer/issues> with a reprex (<https://tidyverse.org/help/>) and the full backtrace.

# token_usage() shows price if available

    Code
      token_usage()
    Output
        provider  model   input output cached_input price
      1   OpenAI gpt-4o 1500000  2e+05            0 $5.75

# dollars looks good, including in data.frames

    Code
      price
    Output
      [1] $1.23
    Code
      data.frame(price)
    Output
        price
      1 $1.23

