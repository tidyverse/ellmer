# api_key is deprecated

    Code
      chat <- chat_openai_compatible_test(api_key = "abc")
    Condition
      Warning:
      The `api_key` argument of `chat_openai_compatible()` is deprecated as of ellmer 0.4.0.
      i Please use the `credentials` argument instead.

# errors if both credentials and api_key are provided

    Code
      chat_openai_compatible_test(credentials = "abc", api_key = "def")
    Condition
      Error in `chat_openai_compatible()`:
      ! Must supply one of `api_key` or `credentials`.

# verifies all properties of credentials

    Code
      chat_openai_test(credentials = 1)
    Condition
      Error in `chat_openai()`:
      ! `credentials` must be a function or `NULL`, not the number 1.
    Code
      chat_openai_test(credentials = function(a, b) a + b)
    Condition
      Error in `chat_openai()`:
      ! `credentials` must not have arguments.
    Code
      chat_openai_test(credentials = function() 1)
    Condition
      Error in `chat_openai()`:
      ! `credentials()` must be a string or a named list, not the number 1.

