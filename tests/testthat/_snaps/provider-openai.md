# defaults are reported

    Code
      . <- chat_openai()
    Message
      Using model = "gpt-5.6-terra".

# file_upload() validates expires_in_h

    Code
      file_upload(provider, path, expires_in_h = 0.5)
    Condition
      Error in `method(file_upload, ellmer::ProviderOpenAI)`:
      ! `expires_in_h` must be a number between 1 and 720, not the number 0.5.
    Code
      file_upload(provider, path, expires_in_h = 31 * 24)
    Condition
      Error in `method(file_upload, ellmer::ProviderOpenAI)`:
      ! `expires_in_h` must be a number between 1 and 720, not the number 744.

