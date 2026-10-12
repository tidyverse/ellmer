# as_json specialised for OpenAI

    Code
      as_json(stub, type_object(.additional_properties = TRUE))
    Condition
      Error in `method(as_json, list(ellmer::ProviderOpenAICompatible, ellmer::TypeObject))`:
      ! `.additional_properties` not supported for OpenAI.

# as_json() references uploaded documents but rejects images

    Code
      as_json(provider, ContentUploaded("file-1", "image/png"))
    Condition
      Error in `method(as_json, list(ellmer::ProviderOpenAICompatible, ellmer::ContentUploaded))`:
      ! The Chat Completions API can't reference an uploaded image by id.
      i Send the image inline with `content_image_file()` instead.

