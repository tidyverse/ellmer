# handles errors

    Code
      chat$chat("What is 1 + 1?", echo = FALSE)
    Condition
      Error in `method(value_turn, ellmer::ProviderOpenRouter)`:
      ! message
    Code
      chat$chat("What is 1 + 1?", echo = TRUE)
    Condition
      Error in `method(stream_parse, ellmer::ProviderOpenRouter)`:
      ! message
