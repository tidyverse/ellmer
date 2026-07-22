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
      Using model = "us.anthropic.claude-sonnet-4-6".

# can use images

    Code
      . <- chat$chat("What's in this image?", image_remote)
    Condition
      Error in `method(as_json, list(ellmer::ProviderAWSBedrock, ellmer::ContentImageRemote))`:
      ! Bedrock doesn't support remote images

# invalid api and cache combinations are rejected

    Code
      chat_aws_bedrock(model = "openai.gpt-5.4", cache = "5m")
    Condition
      Error in `chat_aws_bedrock()`:
      ! `cache` is not supported when `api = "responses"`.
      i The Responses API caches prompts automatically.
    Code
      chat_aws_bedrock(api = "mantle")
    Condition
      Error in `chat_aws_bedrock()`:
      ! `api` must be one of "converse", "messages", or "responses", not "mantle".

