# defaults are reported

    Code
      . <- chat_claude()
    Message
      Using model = "claude-3-7-sonnet-latest".

# all tool variations work

    Code
      chat$chat("Great. Do it again.")
    Condition
      Error in `FUN()`:
      ! Can't use async tools with `$chat()` or `$stream()`.
      i Async tools are supported, but you must use `$chat_async()` or `$stream_async()`.

# max_tokens is deprecated

    Code
      chat <- chat_claude_test(max_tokens = 10)
    Condition
      Warning:
      The `max_tokens` argument of `chat_claude()` is deprecated as of ellmer 0.2.0.
      i Please use the `params` argument instead.

