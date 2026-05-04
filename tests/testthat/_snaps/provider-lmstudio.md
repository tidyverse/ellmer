# includes list of models in error message if `model` is missing

    Code
      chat_lmstudio()
    Condition
      Error in `chat_lmstudio()`:
      ! Must specify `model`.
      i Locally available models: "llama3" and "google/gemma-4-26b-a4b".

# checks that requested model is loaded

    Code
      chat_lmstudio(model = "not-a-real-model")
    Condition
      Error in `chat_lmstudio()`:
      ! Model "not-a-real-model" is not available in LM Studio.
      i Download the model using the LM Studio GUI.
      i See locally available models with `ellmer::models_lmstudio()`.

