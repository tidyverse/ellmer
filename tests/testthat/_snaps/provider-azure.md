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
      req
    Message
      <httr2_request>
      POST
      https://ai-hwickhamai260967855527.openai.azure.com/openai/deployments/gpt-4o-mini/chat/completions?api-version=2024-06-01
      Headers:
      * api-key: "<REDACTED>"
      Body: json encoded data
      Policies:
      * retry_max_tries: 2
      * retry_on_failure: FALSE
      * retry_failure_threshold: Inf
      * retry_failure_timeout: 30
      * retry_realm: "ai-hwickhamai260967855527.openai.azure.com"
      * error_body: a function

---

    Code
      req
    Message
      <httr2_request>
      POST
      https://ai-hwickhamai260967855527.openai.azure.com/openai/deployments/gpt-4o-mini/chat/completions?api-version=2024-06-01
      Headers:
      * Authorization: "<REDACTED>"
      Body: json encoded data
      Policies:
      * retry_max_tries: 2
      * retry_on_failure: FALSE
      * retry_failure_threshold: Inf
      * retry_failure_timeout: 30
      * retry_realm: "ai-hwickhamai260967855527.openai.azure.com"
      * error_body: a function

---

    Code
      req
    Message
      <httr2_request>
      POST
      https://ai-hwickhamai260967855527.openai.azure.com/openai/deployments/gpt-4o-mini/chat/completions?api-version=2024-06-01
      Headers:
      * api-key: "<REDACTED>"
      * Authorization: "<REDACTED>"
      Body: json encoded data
      Policies:
      * retry_max_tries: 2
      * retry_on_failure: FALSE
      * retry_failure_threshold: Inf
      * retry_failure_timeout: 30
      * retry_realm: "ai-hwickhamai260967855527.openai.azure.com"
      * error_body: a function

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
      
      

