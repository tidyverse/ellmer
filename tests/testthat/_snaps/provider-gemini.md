# defaults are reported

    Code
      . <- chat_google_gemini()
    Message
      Using model = "gemini-2.0-flash".

# can use images

    Code
      . <- chat$chat("What's in this image?", image_remote)
    Condition
      Error in `method(as_json, list(ellmer::ProviderGoogleGemini, ellmer::ContentImageRemote))`:
      ! Gemini doesn't support remote images

