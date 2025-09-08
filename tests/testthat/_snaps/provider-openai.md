# defaults are reported

    Code
      . <- chat_openai()
    Message
      Using model = "gpt-4.1".

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

