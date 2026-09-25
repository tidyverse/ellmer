# stream_merge_chunks() rebuilds the interaction from events

    Code
      stream_merge_chunks(provider, result, stream_event("error", error = list(code = "api_error",
        message = "Something went wrong")))
    Condition
      Error in `method(stream_merge_chunks, ellmer::ProviderGoogleGemini)`:
      ! Request failed (api_error)
      Something went wrong

