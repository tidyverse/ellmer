# defaults are reported

    Code
      . <- chat_databricks()
    Message
      Using model = "databricks-claude-3-7-sonnet".

# M2M authentication requests look correct

    Code
      str(request_summary(req))
    Output
      List of 3
       $ url    : chr "https://example.cloud.databricks.com/oidc/v1/token"
       $ headers:List of 2
        ..$ Authorization: chr "Basic aWQ6c2VjcmV0"
        ..$ Accept       : chr "application/json"
       $ body   :List of 2
        ..$ grant_type: 'AsIs' chr "client_credentials"
        ..$ scope     : 'AsIs' chr "all-apis"

