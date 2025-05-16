# includes list of models in error message if `model` is missing

    Code
      chat_ollama()
    Condition
      Error in `chat_ollama()`:
      ! Must specify `model`.
      i Locally installed models: "llama3".

# checks that requested model is installed

    Code
      chat_ollama(model = "not-a-real-model")
    Condition
      Error in `chat_ollama()`:
      ! Model "not-a-real-model" is not installed locally.
      i Run `ollama pull not-a-real-model` in your terminal or `ollamar::pull("not-a-real-model")` in R to install the model.
      i See locally installed models with `ellmer::models_ollama()`.

# as_json specialised for Ollama

    Code
      as_json(stub, type_object(.additional_properties = TRUE))
    Condition
      Error in `method(as_json, list(ellmer::ProviderOllama, ellmer::TypeObject))`:
      ! `.additional_properties` not supported for Ollama.

