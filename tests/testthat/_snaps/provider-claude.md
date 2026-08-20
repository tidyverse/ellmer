# defaults are reported

    Code
      . <- chat_anthropic()
    Message
      Using model = "claude-sonnet-5".

# binary documents are rejected

    Code
      as_json(provider, docx)
    Condition
      Error in `method(as_json, list(ellmer::ProviderAnthropic, ellmer::ContentDocument))`:
      ! Anthropic doesn't support "application/vnd.openxmlformats-officedocument.wordprocessingml.document" documents.
      i Convert the document to plain text or PDF first.

