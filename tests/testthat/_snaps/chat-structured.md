# useful error if no ContentJson

    Code
      extract_data(turn)
    Condition
      Error in `extract_data()`:
      ! Data extraction failed: no JSON responses found.

# warns if multiple ContentJson (and uses first)

    Code
      result <- extract_data(turn, type)
    Condition
      Warning:
      Found 3 JSON responses, using the first.

# type_object(.additional_properties) is deprecated

    Code
      . <- type_object(.additional_properties = TRUE)
    Condition
      Warning:
      The `.additional_properties` argument of `type_object()` is deprecated as of ellmer 0.5.0.

