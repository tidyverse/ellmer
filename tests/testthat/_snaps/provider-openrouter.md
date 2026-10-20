# handles errors

    Code
      chat$chat("What is 1 + 1?", echo = FALSE)
    Condition
      Error in `req_perform()`:
      ! HTTP 400 Bad Request.
      i temperature: Invalid input: expected number, received string
    Code
      chat$chat("What is 1 + 1?", echo = TRUE)
    Condition
      Error in `req_perform_connection()`:
      ! HTTP 400 Bad Request.
      i temperature: Invalid input: expected number, received string

# value_turn() preserves error checking

    Code
      value_turn(provider, model, result)
    Condition
      Error in `method(value_turn, ellmer::ProviderOpenRouter)`:
      ! Provider returned error
      i {
          "message": "boom"
      }

# model listing fails normally on request errors

    Code
      models_openrouter()
    Condition
      Error in `req_perform()`:
      ! Failed to connect to OpenRouter

