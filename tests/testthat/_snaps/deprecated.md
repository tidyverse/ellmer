# chat_azure() is deprecated

    Code
      . <- chat_azure("foo", "bar", api_key = "key")
    Condition
      Warning:
      `chat_azure()` was deprecated in ellmer 0.2.0.
      i Please use `chat_azure_openai()` instead.
    Message
      Using api_version = "2024-10-21".
    Condition
      Warning:
      The `api_key` argument of `chat_azure_openai()` is deprecated as of ellmer 0.4.0.
      i Please use the `credentials` argument instead.

# chat_gemini() is deprecated

    Code
      . <- chat_gemini(api_key = "key")
    Condition
      Warning:
      `chat_gemini()` was deprecated in ellmer 0.2.0.
      i Please use `chat_google_gemini()` instead.
    Message
      Using model = "gemini-2.5-flash".
    Condition
      Warning:
      The `api_key` argument of `chat_google_gemini()` is deprecated as of ellmer 0.4.0.
      i Please use the `credentials` argument instead.

