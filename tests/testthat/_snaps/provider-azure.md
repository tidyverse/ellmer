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

