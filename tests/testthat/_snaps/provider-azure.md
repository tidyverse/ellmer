# defaults are reported

    Code
      . <- chat_azure_test()
    Message
      Using api_version = "2024-10-21".

# all tool variations work

    Code
      chat$chat("Great. Do it again.")
    Condition
      Error in `FUN()`:
      ! Can't use async tools with `$chat()` or `$stream()`.
      i Async tools are supported, but you must use `$chat_async()` or `$stream_async()`.

# Azure request headers are generated correctly

    Code
      req$headers
    Output
      $`api-key`
      [1] "key"
      
      attr(,"redact")
      [1] "api-key"       "Authorization"

---

    Code
      req$headers
    Output
      $Authorization
      [1] "Bearer token"
      
      attr(,"redact")
      [1] "Authorization"

---

    Code
      req$headers
    Output
      $`api-key`
      [1] "key"
      
      $Authorization
      [1] "Bearer token"
      
      attr(,"redact")
      [1] "api-key"       "Authorization"

# service principal authentication requests look correct

    Code
      list(url = req$url, headers = req$headers, body = req$body$data)
    Output
      $url
      [1] "https://login.microsoftonline.com/aaaa0a0a-bb1b-cc2c-dd3d-eeeeee4e4e4e/oauth2/v2.0/token"
      
      $headers
      $headers$Accept
      [1] "application/json"
      
      attr(,"redact")
      character(0)
      
      $body
      $body$grant_type
      [1] "client_credentials"
      
      $body$scope
      [1] "https%3A%2F%2Fcognitiveservices.azure.com%2F.default"
      
      $body$client_id
      [1] "id"
      
      $body$client_secret
      [1] "secret"
      
      

