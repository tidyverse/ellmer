# defaults are reported

    Code
      . <- chat_databricks()
    Message
      Using model = "databricks-claude-sonnet-5".

# M2M authentication requests look correct

    Code
      request_summary(req)
    Output
      $url
      [1] "https://example.cloud.databricks.com/oidc/v1/token"
      
      $headers
      $headers$Authorization
      [1] "Basic aWQ6c2VjcmV0"
      
      $headers$Accept
      [1] "application/json"
      
      
      $body
      $body$grant_type
      [1] "client_credentials"
      
      $body$scope
      [1] "all-apis"
      
      

