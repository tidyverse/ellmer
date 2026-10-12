# file_upload() validates expires_in_h

    Code
      file_upload(provider, path, expires_in_h = 0.5)
    Condition
      Error in `method(file_upload, ellmer::ProviderAnthropic)`:
      ! `expires_in_h` must be a number between 1 and 2160, not the number 0.5.
    Code
      file_upload(provider, path, expires_in_h = 91 * 24)
    Condition
      Error in `method(file_upload, ellmer::ProviderAnthropic)`:
      ! `expires_in_h` must be a number between 1 and 2160, not the number 2184.

# claude_file_upload() is deprecated

    Code
      . <- claude_file_upload(test_path("apples.pdf"))
    Condition
      Warning:
      `claude_file_upload()` was deprecated in ellmer 0.5.0.
      i Please use `Chat$file_upload()` instead.

