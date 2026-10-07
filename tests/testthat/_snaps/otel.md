# request errors are recorded on spans and metrics

    Code
      chat$chat("hi")
    Condition
      Error in `chat_perform()`:
      ! boom

# duration is recorded once when parsing fails after the response

    Code
      chat$chat("hi")
    Condition
      Error in `value_turn()`:
      ! bad response

