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

