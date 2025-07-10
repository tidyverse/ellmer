# M2M authentication requests look correct

    Code
      list(url = req$url, headers = req_get_headers(req, "reveal"), body = req_get_body(
        req))
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
      
      

