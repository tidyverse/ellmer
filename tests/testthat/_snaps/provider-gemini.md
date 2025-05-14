# defaults are reported

    Code
      . <- chat_google_gemini()
    Message
      Using model = "gemini-2.0-flash".

# all tool variations work

    Code
      chat$chat("Great. Do it again.")
    Condition
      Error:
      ! Can't use async tools with `$chat()` or `$stream()`.
      i Async tools are supported, but you must use `$chat_async()` or `$stream_async()`.

# can use images

    Code
      . <- chat$chat("What's in this image?", image_remote)
    Condition
      Error in `method(as_json, list(ellmer::ProviderGoogleGemini, ellmer::ContentImageRemote))`:
      ! Gemini doesn't support remote images

