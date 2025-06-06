# defaults are reported

    Code
      . <- chat_snowflake()
    Message
      Using model = "claude-3-7-sonnet".

# all tool variations work

    Code
      chat$chat("Great. Do it again.")
    Condition
      Error:
      ! Can't use async tools with `$chat()` or `$stream()`.
      i Async tools are supported, but you must use `$chat_async()` or `$stream_async()`.

