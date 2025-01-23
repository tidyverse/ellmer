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

# all tool variations work

    Code
      chat$chat("Great. Do it again.")
    Condition
      Error in `FUN()`:
      ! Can't use async tools with `$chat()` or `$stream()`.
      i Async tools are supported, but you must use `$chat_async()` or `$stream_async()`.

