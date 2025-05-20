# defaults are reported

    Code
      . <- chat_openai()
    Message
      Using model = "gpt-4.1".

# all tool variations work

    Code
      chat$chat("Great. Do it again.")
    Condition
      Error:
      ! Can't use async tools with `$chat()` or `$stream()`.
      i Async tools are supported, but you must use `$chat_async()` or `$stream_async()`.

# as_json specialised for OpenAI

    Code
      as_json(stub, type_object(.additional_properties = TRUE))
    Condition
      Error in `method(as_json, list(ellmer::ProviderOpenAI, ellmer::TypeObject))`:
      ! `.additional_properties` not supported for OpenAI.

# seed is deprecated, but still honored

    Code
      chat <- chat_openai_test(seed = 1)
    Condition
      Warning:
      The `seed` argument of `chat_openai()` is deprecated as of ellmer 0.2.0.
      i Please use the `params` argument instead.

