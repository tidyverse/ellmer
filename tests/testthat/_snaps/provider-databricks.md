# defaults are reported

    Code
      . <- chat_databricks()
    Message
      Using model = "databricks-claude-3-7-sonnet".

# all tool variations work

    Code
      chat$chat("Great. Do it again.")
    Condition
      Error:
      ! Can't use async tools with `$chat()` or `$stream()`.
      i Async tools are supported, but you must use `$chat_async()` or `$stream_async()`.

# M2M authentication requests look correct

    Code
      list(url = req$url, headers = req$headers, body = req$body$data)
    Output
      $url
      [1] "https://example.cloud.databricks.com/oidc/v1/token"
      
      $headers
      <httr2_headers>
      Authorization: <REDACTED>
      Accept: application/json
      
      $body
      $body$grant_type
      [1] "client_credentials"
      
      $body$scope
      [1] "all-apis"
      
      

