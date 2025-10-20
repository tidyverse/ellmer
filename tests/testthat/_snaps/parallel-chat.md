# errors in conversion become warnings

    Code
      out <- multi_convert(provider, turns, type = type)
    Condition
      Warning:
      Failed to extract data from 2/3 turns
      * 2: Data extraction failed: no JSON responses found.
      * 3: parse error: premature EOF { (right here) ------^

