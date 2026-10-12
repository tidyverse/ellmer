# file operations error on Vertex

    Code
      file_upload(provider, "apples.pdf")
    Condition
      Error in `method(file_upload, ellmer::ProviderGoogleGemini)`:
      ! The Gemini Files API is not available on Vertex AI.
      i Upload the file to a Cloud Storage bucket and reference it with `ContentUploaded(uri = "gs://bucket/object", mime_type = ...)`.

# file_upload() rejects expires_in_h other than 48 hours

    Code
      file_upload(provider, path, expires_in_h = 1)
    Condition
      Error in `method(file_upload, ellmer::ProviderGoogleGemini)`:
      ! Gemini files always expire after 48 hours, so `expires_in_h` must be 48.
    Code
      file_upload(provider, path, expires_in_h = Inf)
    Condition
      Error in `method(file_upload, ellmer::ProviderGoogleGemini)`:
      ! Gemini files always expire after 48 hours, so `expires_in_h` must be 48.

# google_upload() is deprecated

    Code
      . <- google_upload(test_path("apples.pdf"))
    Condition
      Warning:
      `google_upload()` was deprecated in ellmer 0.5.0.
      i Please use `Chat$file_upload()` instead.

