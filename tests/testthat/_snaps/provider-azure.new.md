# Azure request headers are generated correctly

    Code
      str(req$headers)
    Output
       <httr2_headers>
       $ api-key: <REDACTED>

---

    Code
      str(req$headers)
    Output
       <httr2_headers>
       $ Authorization: <REDACTED>

---

    Code
      str(req$headers)
    Output
       <httr2_headers>
       $ api-key      : <REDACTED>
       $ Authorization: <REDACTED>

# service principal authentication requests look correct

    Code
      list(url = req$url, headers = req$headers, body = req$body$data)
    Output
      $url
      [1] "https://login.microsoftonline.com/aaaa0a0a-bb1b-cc2c-dd3d-eeeeee4e4e4e/oauth2/v2.0/token"
      
      $headers
      <httr2_headers>
      Accept: application/json
      
      $body
      $body$grant_type
      [1] "client_credentials"
      
      $body$scope
      [1] "https%3A%2F%2Fcognitiveservices.azure.com%2F.default"
      
      $body$client_id
      [1] "id"
      
      $body$client_secret
      [1] "secret"
      
      

