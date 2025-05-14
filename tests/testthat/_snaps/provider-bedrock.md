# handles errors

    Code
      chat$chat("What is 1 + 1?", echo = FALSE)
    Condition
      Error in `req_perform()`:
      ! HTTP 400 Bad Request.
      * STRING_VALUE cannot be converted to Float
    Code
      chat$chat("What is 1 + 1?", echo = TRUE)
    Condition
      Error in `req_perform_connection()`:
      ! HTTP 400 Bad Request.
      * STRING_VALUE cannot be converted to Float

# defaults are reported

    Code
      . <- chat_aws_bedrock()
    Message
      Using model = "anthropic.claude-3-5-sonnet-20240620-v1:0".

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
      Error in `method(as_json, list(ellmer::ProviderAWSBedrock, ellmer::ContentImageRemote))`:
      ! Bedrock doesn't support remote images

