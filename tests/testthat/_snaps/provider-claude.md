# defaults are reported

    Code
      . <- chat_anthropic()
    Message
      Using model = "claude-sonnet-4-5-20250929".

# chat_body() warns when allowed_callers is set but no code tool is registered

    Code
      . <- chat_body(provider, turns = list(UserTurn("hi")), tools = list(f = programmatic))
    Condition
      Warning:
      A tool sets `allowed_callers` but no code execution tool is registered.
      i Register `claude_tool_code_execution()` to enable programmatic tool calling.
      i Until then, the tool can only be called directly.
      This warning is displayed once every 8 hours.

