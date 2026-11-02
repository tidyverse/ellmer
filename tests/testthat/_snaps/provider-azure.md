# defaults are reported

    Code
      . <- chat_azure_openai("endpoint", "deployment_id")
    Message
      Using api_version = "2024-10-21".

# Azure request headers are generated correctly

    Code
      str(req_get_headers(req, "reveal"))
    Output
      List of 1
       $ api-key: chr "key"

# service principal authentication requests look correct

    Code
      request_summary(req)
    Output
      $url
      [1] "https://login.microsoftonline.com/aaaa0a0a-bb1b-cc2c-dd3d-eeeeee4e4e4e/oauth2/v2.0/token"
      
      $headers
      $headers$Accept
      [1] "application/json"
      
      
      $body
      $body$grant_type
      [1] "client_credentials"
      
      $body$scope
      [1] "https%3A%2F%2Fcognitiveservices.azure.com%2F.default"
      
      $body$client_id
      [1] "id"
      
      $body$client_secret
      [1] "secret"
      
      

