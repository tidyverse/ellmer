# useful errors

    Code
      chat()
    Condition
      Error in `chat()`:
      ! `name` must be a single string, not absent.
    Code
      chat("a/b/c")
    Condition
      Error in `chat()`:
      ! `name` must be in form "provider" or "provider/model".
    Code
      chat("susan")
    Condition
      Error in `chat()`:
      ! Can't find provider `ellmer::chat_susan()`.
    Code
      chat("susan/jones")
    Condition
      Error in `chat()`:
      ! Can't find provider `ellmer::chat_susan()`.

# warns if given arguments that aren't used by provider

    Code
      chat("openai/gpt-4.1-mini", foo = "bar")
    Condition
      Warning:
      Ignoring `foo` argument that is not used by `ellmer::chat_openai()`.
    Output
      <Chat OpenAI/gpt-4.1-mini turns=0 tokens=0/0 $0.00>

---

    Code
      chat(paste0("ollama/", model), params = params(temperature = 0.5))
    Condition
      Warning:
      Ignoring `params` argument that is not used by `ellmer::chat_ollama()`.
    Output
      <Chat Ollama/qwen3:4b turns=0 tokens=0/0>

# requires `model` and `system_prompt` arguments

    Code
      chat("cortex_analyst")
    Condition
      Error in `chat()`:
      ! `ellmer::chat()` does not support `ellmer::chat_cortex_analyst()`, please call it directly.

