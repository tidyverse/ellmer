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

# overwriting a tool yields a message

    Code
      chat$register_tool(my_tool)
    Message
      Replacing existing my_tool tool.

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

# invoke_tools() echoes tool requests and results

    Code
      . <- coro::collect(invoke_tools(turn, echo = "output"))
    Message
      ( ) [tool call] my_tool()
      o #> 1
      ( ) [tool call] my_tool(x = 1)
      # #> Error: Unused argument: x
      ( ) [tool call] tool_list()
      o #> {"a":1,"b":2}
      ( ) [tool call] tool_chr()
      o #> a
        #> b
        #> c
      ( ) [tool call] tool_abort()
      # #> Error: Unexpected input
        #> i Please revise and try again.

# invoke_tools_async() echoes tool requests and results

    Code
      . <- sync(gen_async_promise_all(invoke_tools_async(turn, echo = "output")))
    Message
      ( ) [tool call] my_tool()
      ( ) [tool call] my_tool(x = 1)
      ( ) [tool call] tool_list()
      ( ) [tool call] tool_chr()
      ( ) [tool call] tool_abort()
      # #> Error: Unused argument: x
      # #> Error: Unexpected input
        #> i Please revise and try again.
      o #> 1
      o #> {"a":1,"b":2}
      o #> a
        #> b
        #> c
    Code
      . <- sync(coro::async_collect(invoke_tools_async(turn, echo = "output")))
    Message
      ( ) [tool call] my_tool()
      o #> 1
      ( ) [tool call] my_tool(x = 1)
      # #> Error: Unused argument: x
      ( ) [tool call] tool_list()
      o #> {"a":1,"b":2}
      ( ) [tool call] tool_chr()
      o #> a
        #> b
        #> c
      ( ) [tool call] tool_abort()
      # #> Error: Unexpected input
        #> i Please revise and try again.

# tool error warnings

    Code
      warn_tool_errors(errors)
    Condition
      Warning:
      Failed to evaluate 2 tool calls.
      x [returns_json (call1)]: The JSON was invalid: {[1, 2, 3]}
      x [throws (call2)]: went boom!

