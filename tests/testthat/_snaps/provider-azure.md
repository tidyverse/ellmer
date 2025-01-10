# Azure request headers are generated correctly

    Code
      req
    Message
      <httr2_request>
      POST
      https://ai-hwickhamai260967855527.openai.azure.com/openai/deployments/gpt-4o-mini/chat/completions?api-version=2024-06-01
      Headers:
      * api-key: '<REDACTED>'
      Body: json encoded data
      Policies:
      * retry_max_tries: 2
      * retry_on_failure: FALSE
      * error_body: a function

---

    Code
      req
    Message
      <httr2_request>
      POST
      https://ai-hwickhamai260967855527.openai.azure.com/openai/deployments/gpt-4o-mini/chat/completions?api-version=2024-06-01
      Headers:
      * Authorization: '<REDACTED>'
      Body: json encoded data
      Policies:
      * retry_max_tries: 2
      * retry_on_failure: FALSE
      * error_body: a function

