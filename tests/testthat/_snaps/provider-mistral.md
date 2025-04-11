# can handle errors

    Code
      chat$chat("Hi")
    Condition
      Error in `req_perform()`:
      ! HTTP 400 Bad Request.
      * Invalid model: doesnt-exist

# defaults are reported

    Code
      . <- chat_mistral()
    Message
      Using model = "mistral-large-latest".

