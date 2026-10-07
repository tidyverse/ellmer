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

# unsupported request params are not recorded on spans

    Code
      . <- chat$chat("hi", echo = "none")
    Condition
      Warning:
      Ignoring unsupported parameters: "reasoning_effort"

