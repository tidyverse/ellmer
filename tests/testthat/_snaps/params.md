# checks its inputs

    Code
      params(temperature = "x")
    Condition
      Error in `params()`:
      ! `temperature` must be a number or `NULL`, not the string "x".
    Code
      params(top_p = "x")
    Condition
      Error in `params()`:
      ! `top_p` must be a number or `NULL`, not the string "x".
    Code
      params(top_k = "x")
    Condition
      Error in `params()`:
      ! `top_k` must be a whole number or `NULL`, not the string "x".
    Code
      params(frequency_penalty = "x")
    Condition
      Error in `params()`:
      ! `frequency_penalty` must be a number or `NULL`, not the string "x".
    Code
      params(presence_penalty = "x")
    Condition
      Error in `params()`:
      ! `presence_penalty` must be a number or `NULL`, not the string "x".
    Code
      params(seed = "x")
    Condition
      Error in `params()`:
      ! `seed` must be a whole number or `NULL`, not the string "x".
    Code
      params(max_tokens = "x")
    Condition
      Error in `params()`:
      ! `max_tokens` must be a whole number or `NULL`, not the string "x".
    Code
      params(log_probs = 1)
    Condition
      Error in `params()`:
      ! `log_probs` must be `TRUE`, `FALSE`, or `NULL`, not the number 1.
    Code
      params(stop_sequences = 1)
    Condition
      Error in `params()`:
      ! `stop_sequences` must be a character vector or `NULL`, not the number 1.

# standardise_params warns about unknown args

    Code
      . <- standardise_params(test_params, provider_params)
    Condition
      Warning:
      Ignoring unsupported parameters: "top_p"

