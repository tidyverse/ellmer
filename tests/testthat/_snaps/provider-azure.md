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

---

    Code
      str(req_get_headers(req, "reveal"))
    Output
      List of 1
       $ Authorization: chr "Bearer token"

---

    Code
      str(req_get_headers(req, "reveal"))
    Output
      List of 2
       $ api-key      : chr "key"
       $ Authorization: chr "Bearer token"

# service principal authentication requests look correct

    Code
      str(request_summary(req))
    Output
      List of 3
       $ url    : chr "https://login.microsoftonline.com/aaaa0a0a-bb1b-cc2c-dd3d-eeeeee4e4e4e/oauth2/v2.0/token"
       $ headers:List of 1
        ..$ Accept: chr "application/json"
       $ body   :List of 4
        ..$ grant_type   : 'AsIs' chr "client_credentials"
        ..$ scope        : 'AsIs' chr "https%3A%2F%2Fcognitiveservices.azure.com%2F.default"
        ..$ client_id    : 'AsIs' chr "id"
        ..$ client_secret: 'AsIs' chr "secret"

