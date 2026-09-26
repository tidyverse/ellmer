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
      multi_convert(provider, turns, type = type, include_tokens = TRUE)
    Condition
      Warning:
      `include_tokens` is ignored because the result is not a data frame.
    Output
      [[1]]
      # A tibble: 1 x 2
        text  label
        <chr> <chr>
      1 a     x    
      

---

    Code
      multi_convert(provider, turns, type = type, include_cost = TRUE)
    Condition
      Warning:
      `include_cost` is ignored because the result is not a data frame.
    Output
      [[1]]
      # A tibble: 1 x 2
        text  label
        <chr> <chr>
      1 a     x    
      

---

    Code
      multi_convert(provider, turns, type = type, include_tokens = TRUE,
        include_cost = TRUE)
    Condition
      Warning:
      `include_tokens` and `include_cost` are ignored because the result is not a data frame.
    Output
      [[1]]
      # A tibble: 1 x 2
        text  label
        <chr> <chr>
      1 a     x    
      

