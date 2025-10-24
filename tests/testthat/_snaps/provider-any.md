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

