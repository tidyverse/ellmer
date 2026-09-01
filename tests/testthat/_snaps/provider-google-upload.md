# file operations error on Vertex

    Code
      file_upload(provider, "apples.pdf")
    Condition
      Error in `method(file_upload, ellmer::ProviderGoogleGemini)`:
      ! The Gemini Files API is not available on Vertex AI.
      i Upload the file to a Cloud Storage bucket and reference it with `ContentUploaded(uri = "gs://bucket/object", mime_type = ...)`.

