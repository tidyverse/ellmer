# content is serialized as Interactions blocks

    Code
      as_json(provider, ContentImageRemote("https://example.com/image"))
    Condition
      Error in `method(as_json, list(ellmer::ProviderGoogleInteractions, ellmer::ContentImageRemote))`:
      ! Can't guess the type of the image at <https://example.com/image> from its URL.
      i Download the image and use `content_image_file()` instead.

# stream_merge_chunks() rebuilds the interaction from events

    Code
      stream_merge_chunks(provider, result, stream_event("error", error = list(code = "api_error",
        message = "Something went wrong")))
    Condition
      Error in `method(stream_merge_chunks, ellmer::ProviderGoogleInteractions)`:
      ! Request failed (api_error)
      Something went wrong

