# validate_price_providers() names missing providers

    Code
      validate_price_providers(df, EXPECTED_PRICE_PROVIDERS)
    Condition
      Error in `validate_price_providers()`:
      x Expected providers missing from the pricing data: "AWS/Bedrock", "Azure/OpenAI", "Google/Gemini", "Google/Vertex", "Groq", "Mistral", and "Posit"

# validate_price_providers() detects a same-count substitution

    Code
      validate_price_providers(df, EXPECTED_PRICE_PROVIDERS)
    Condition
      Error in `validate_price_providers()`:
      x Expected provider missing from the pricing data: "OpenAI"
      x Unexpected provider present in the pricing data: "OpenRouter"

# validate_price_providers() names unexpected providers

    Code
      validate_price_providers(df, EXPECTED_PRICE_PROVIDERS)
    Condition
      Error in `validate_price_providers()`:
      x Unexpected provider present in the pricing data: "OpenRouter"

