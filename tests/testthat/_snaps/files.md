# as_file_id() accepts an id string or a ContentUploaded

    Code
      as_file_id(1)
    Condition
      Error:
      ! `id` must be a single string, not the number 1.

# uploaded files error for providers without support

    Code
      as_json(provider, ContentUploaded("file_123", "application/pdf"))
    Condition
      Error in `method(as_json, list(ellmer::Provider, ellmer::ContentUploaded))`:
      ! test doesn't support uploaded files.

