# includes list of models in error message if `model` is missing

    Code
      chat_ollama()
    Condition
      Error in `chat_ollama()`:
      ! Must specify `model`.
      i Locally installed models: "llama3.2:1b".

# as_json specialised for Ollama

    Code
      as_json(stub, type_object(.additional_properties = TRUE))
    Condition
      Error in `method(as_json, list(ellmer::ProviderOllama, ellmer::TypeObject))`:
      ! `.additional_properties` not supported for Ollama.

