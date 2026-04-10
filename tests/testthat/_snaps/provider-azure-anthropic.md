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

