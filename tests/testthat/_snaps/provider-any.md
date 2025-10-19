# useful errors

    Code
      chat()
    Condition
      Error in `chat()`:
      ! `name` must be a single string, not absent.
    Code
      chat("")
    Condition
      Error in `chat()`:
      ! `name` must be a single string, not the empty string "".
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

# requires `model` and `system_prompt` arguments

    Code
      chat("cortex_analyst")
    Condition
      Error in `chat()`:
      ! `ellmer::chat()` does not support `ellmer::chat_cortex_analyst()`, please call it directly.

