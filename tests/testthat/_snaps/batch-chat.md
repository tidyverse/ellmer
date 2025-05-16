# errors if chat/provider/prompts don't match previous run

    Code
      batch_chat(chat, prompts, path)
    Condition
      Error in `batch_chat()`:
      ! provider, prompts, and user_turns don't match stored values.
      i Do you need to pick a different `path`?

# informative error for bad inputs

    Code
      batch_chat("x")
    Condition
      Error in `batch_chat()`:
      ! `chat` must be a <Chat> object.
    Code
      batch_chat(chat_ollama)
    Condition
      Error in `batch_chat()`:
      ! Batch requests are not currently supported by this provider.
    Code
      batch_chat(chat_openai, "a")
    Condition
      Error in `batch_chat()`:
      ! `prompts` must be a list or prompt, not the string "a".
    Code
      batch_chat(chat_openai, list("a"), path = 1)
    Condition
      Error in `batch_chat()`:
      ! `path` must be a single string, not the number 1.
    Code
      batch_chat(chat_openai, list("a"), path = "x", wait = 1)
    Condition
      Error in `batch_chat()`:
      ! `wait` must be `TRUE` or `FALSE`, not the number 1.

