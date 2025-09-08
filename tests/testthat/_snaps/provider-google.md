# can handle errors

    Code
      chat$chat("Hi")
    Condition
      Error in `req_perform()`:
      ! HTTP 404 Not Found.
      i models/doesnt-exist is not found for API version v1beta, or is not supported for generateContent. Call ListModels to see the list of available models and their supported methods.

# defaults are reported

    Code
      . <- chat_google_gemini()
    Message
      Using model = "gemini-2.5-flash".

# can use images

    Code
      . <- chat$chat("What's in this image?", image_remote)
    Condition
      Error in `method(as_json, list(ellmer::ProviderGoogleGemini, ellmer::ContentImageRemote))`:
      ! Gemini doesn't support remote images

