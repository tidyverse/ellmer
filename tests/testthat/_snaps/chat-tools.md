# can only set/register tools

    Code
      chat$register_tools(list(tool_def, 1))
    Condition
      Error in `chat$register_tools()`:
      ! `tools[[2]]` must be a <ToolDef>, not the number 1.
    Code
      chat$set_tools(list(tool_def, 1))
    Condition
      Error in `chat$set_tools()`:
      ! `tools[[2]]` must be a <ToolDef>, not the number 1.

# chat warns on tool failures

    Code
      . <- chat$chat("What are Joe, Hadley, Simon, and Tom's favorite colors?")
    Condition
      Warning:
      Failed to evaluate 4 tool calls.
      x [user_favorite_color (ID)]: User denied tool request
      x [user_favorite_color (ID)]: User denied tool request
      x [user_favorite_color (ID)]: User denied tool request
      i ... and 1 more.

# chat callbacks for tool requests/results

    Code
      . <- chat$chat("What are Joe and Hadley's favorite colors?")
    Message
      [1] Tool request: Joe
      [1] Tool result: blue
      [2] Tool request: Hadley
      [2] Tool result: red

---

    Code
      chat$on_tool_request(function(data) NULL)
    Condition
      Error:
      ! `callback` must have the argument `request`; it currently has `data`.
    Code
      chat$on_tool_result(function(data) NULL)
    Condition
      Error:
      ! `callback` must have the argument `result`; it currently has `data`.

