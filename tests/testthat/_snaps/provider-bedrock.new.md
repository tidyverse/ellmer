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
      Using model = "anthropic.claude-3-5-sonnet-20241022-v2:0".

