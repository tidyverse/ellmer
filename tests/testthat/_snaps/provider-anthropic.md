# defaults are reported

    Code
      . <- chat_anthropic()
    Message
      Using model = "claude-sonnet-4-20250514".

# max_tokens is deprecated

    Code
      chat <- chat_anthropic_test(max_tokens = 10)
    Condition
      Warning:
      The `max_tokens` argument of `chat_anthropic()` is deprecated as of ellmer 0.2.0.
      i Please use the `params` argument instead.

