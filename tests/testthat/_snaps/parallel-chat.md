# errors in conversion become warnings

    Code
      out <- multi_convert(provider, turns, type = type)
    Condition
      Warning:
      Failed to extract data from 3/4 turns
      * 2: Data extraction failed: 0 data results recieved.
      * 3: Data extraction failed: 2 data results recieved.
      * 4: parse error: premature EOF { (right here) ------^

