# can handle errors

    Code
      chat$chat("Hi")
    Condition
      Error in `req_perform()`:
      ! HTTP 404 Not Found.
      i Model 'doesnt-exist' not found. Did you mean 'gemini-pro-latest'? Please verify the model name against the supported list: https://ai.google.dev/gemini-api/docs/models [not_found]

# defaults are reported

    Code
      . <- chat_google_gemini()
    Message
      Using model = "gemini-3.7-flash".

# binary documents are rejected

    Code
      as_json(provider, xls)
    Condition
      Error in `method(as_json, list(ellmer::ProviderGoogleInteractions, ellmer::ContentDocument))`:
      ! Gemini doesn't support "application/vnd.ms-excel" documents.
      i Convert the document to plain text or PDF first.

