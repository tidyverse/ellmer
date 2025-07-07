# can only register tools

    Code
      chat$register_tool(1)
    Condition
      Error in `chat$register_tool()`:
      ! `tool` must be a <ToolDef>.
    Code
      chat$register_tools(1)
    Condition
      Error in `chat$register_tools()`:
      ! `tools` must be a list, not the number 1.
    Code
      chat$register_tools(list(tool_def, 1))
    Condition
      Error in `chat$register_tools()`:
      ! `tools[[2]]` must be a <ToolDef>, not the number 1.

# chat can get and register a list of tools

    Code
      chat$set_tools(tools[[1]])
    Condition
      Error in `chat$set_tools()`:
      ! `tools` must be a list of tools created with `ellmer::tool()`.
      i Did you mean to call `$register_tool()`?

---

    Code
      chat$set_tools(c(tools, list("foo")))
    Condition
      Error in `chat$set_tools()`:
      ! `tools` must be a list of tools created with `ellmer::tool()`.

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

# tool calls can be rejected via `tool_request` callbacks

    Code
      . <- chat$chat("What are Joe and Hadley's favorite colors?",
        "Write 'Joe ____ Hadley ____'. Use 'unknown' if you don't know.", echo = "output")
    Message
      ( ) [tool call] user_favorite_color(user = "Joe")
      # #> Error: Tool call rejected. Joe denied the request.
      ( ) [tool call] user_favorite_color(user = "Hadley")
      o #> red
    Output
      Joe unknown Hadley red

# tool calls can be rejected via the tool function

    Code
      . <- chat$chat("What are Joe and Hadley's favorite colors?",
        "Write 'Joe ____ Hadley ____'. Use 'unknown' if you don't know.", echo = "output")
    Message
      ( ) [tool call] user_favorite_color(user = "Joe")
      # #> Error: Tool call rejected. The user has chosen to disallow the tool call.
      ( ) [tool call] user_favorite_color(user = "Hadley")
      o #> red
    Output
      Joe unknown Hadley red

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

