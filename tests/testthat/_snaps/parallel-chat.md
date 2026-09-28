# errors in conversion become warnings

    Code
      out <- multi_convert(provider, turns, type = type)
    Condition
      Warning:
      Failed to extract data from 2/3 turns
      * 2: Data extraction failed: no JSON responses found.
      * 3: parse error: premature EOF { (right here) ------^

# incomplete responses are recorded in `.error` (#1126)

    Code
      out <- multi_convert(provider, turns, type = type)
    Condition
      Warning:
      Failed to extract data from 3/4 turns
      * 2: Response was truncated because it hit the `max_tokens` limit. i Increase `max_tokens` to allow the model to generate the full response.
      * 3: Response was filtered by the provider's content moderation policy.
      * 4: Response may be incomplete, unexpected finish reason: who knows.

