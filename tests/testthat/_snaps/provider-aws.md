# handles errors

    Code
      chat$chat("What is 1 + 1?", echo = FALSE)
    Condition
      Error in `req_perform()`:
      ! HTTP 400 Bad Request.
      i STRING_VALUE cannot be converted to Float
    Code
      chat$chat("What is 1 + 1?", echo = TRUE)
    Condition
      Error in `req_perform_connection()`:
      ! HTTP 400 Bad Request.
      i STRING_VALUE cannot be converted to Float

# defaults are reported

    Code
      . <- chat_aws_bedrock()
    Message
      Using model = "anthropic.claude-3-5-sonnet-20240620-v1:0".

# can use images

    Code
      . <- chat$chat("What's in this image?", image_remote)
    Condition
      Error in `method(as_json, list(ellmer::ProviderAWSBedrock, ellmer::ContentImageRemote))`:
      ! Bedrock doesn't support remote images

