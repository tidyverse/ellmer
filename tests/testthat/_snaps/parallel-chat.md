# errors in conversion become warnings

    Code
      out <- multi_convert(provider, turns, type = type)
    Condition
      Warning:
      Failed to extract data from 2/3 turns
      * 2: Data extraction failed: no JSON responses found.
      * 3: parse error: premature EOF { (right here) ------^

# include_tokens/include_cost warn when result isn't a data frame

    Code
      . <- multi_convert(provider, turns, type = type, include_tokens = TRUE,
        include_cost = TRUE)
    Condition
      Warning:
      Can't add token or cost columns to a result that isn't a data frame.
      ! Ignoring `include_tokens` and `include_cost`.
      i Use `type_object()` for `type` and `convert = TRUE` to get a data frame.

