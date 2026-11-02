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

# login url only includes callback and state when supplied

    Code
      openrouter_auth_url(pkce)
    Output
      [1] "https://openrouter.ai/auth?code_challenge=challenge&code_challenge_method=S256&key_label=ellmer"
    Code
      openrouter_auth_url(pkce, state = "state", callback_url = "http://localhost:1410/")
    Output
      [1] "https://openrouter.ai/auth?callback_url=http%3A%2F%2Flocalhost%3A1410%2F&code_challenge=challenge&code_challenge_method=S256&key_label=ellmer&state=state"

# exchanges the code for a key

    Code
      str(request_summary(req))
    Output
      List of 3
       $ url    : chr "https://openrouter.ai/api/v1/auth/keys"
       $ headers: list()
       $ body   :List of 3
        ..$ code                 : chr "code"
        ..$ code_verifier        : chr "verifier"
        ..$ code_challenge_method: chr "S256"

# default credentials prefer the env var, then the cache, then a login

    Code
      default_openrouter_credentials()
    Condition
      Error:
      ! No OpenRouter credentials are available.
      i Set `OPENROUTER_API_KEY`, or call `chat_openrouter()` in an interactive session to log in.

# a revoked cached key is cleared and the user logs in again

    Code
      default_openrouter_credentials()()
    Condition
      Error:
      ! Your saved OpenRouter login is no longer valid.
      i Call `chat_openrouter()` in an interactive session to log in again.

