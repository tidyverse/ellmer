# $chat() echoes tool requests and results

    Code
      chat$chat("What's the current date in Y-M-D format?")
    Message
      ( ) [tool call] tool_001()
      o #> 2024-01-01
    Output
      2024-01-01
    Code
      chat$chat("Ask the user to enter a password")
    Message
      ( ) [tool call] tool_002()
      # #> Error: User denied tool request
    Output
      User denied request
    Code
      chat
    Output
      <Chat OpenAI/gpt-4o-mini turns=9 tokens=433/37 $0.00>
      -- system [0] ------------------------------------------------------------------
      Be very terse, not even punctuation.
      -- user [71] -------------------------------------------------------------------
      What's the current date in Y-M-D format?
      -- assistant [12] --------------------------------------------------------------
      [tool request (ID)]: tool_001()
      -- user [14] -------------------------------------------------------------------
      [tool result  (ID)]: 2024-01-01
      -- assistant [8] ---------------------------------------------------------------
      2024-01-01
      -- user [13] -------------------------------------------------------------------
      Ask the user to enter a password
      -- assistant [12] --------------------------------------------------------------
      [tool request (ID)]: tool_002()
      -- user [17] -------------------------------------------------------------------
      [tool result  (ID)]: Error: User denied tool request
      -- assistant [5] ---------------------------------------------------------------
      User denied request

# invoke_tools() echoes tool requests and results

    Code
      . <- invoke_tools(turn, echo = "output")
    Message
      ( ) [tool call] my_tool()
      o #> 1
      ( ) [tool call] my_tool(x = 1)
      # #> Error: unused argument (x = 1)
      ( ) [tool call] tool_list()
      o #> {{"a":1,"b":2}}
      ( ) [tool call] tool_chr()
      o #> a
        #> b
        #> c
      ( ) [tool call] tool_abort()
      # #> Error: Unexpected input
        #> i Please revise and try again.

# invoke_tools_async() echoes tool requests and results

    Code
      . <- sync(invoke_tools_async(turn, echo = "output"))
    Message
      ( ) [tool call] my_tool()
      ( ) [tool call] my_tool(x = 1)
      ( ) [tool call] tool_list()
      ( ) [tool call] tool_chr()
      ( ) [tool call] tool_abort()
      # #> Error: unused argument (x = 1)
      # #> Error: Unexpected input
        #> i Please revise and try again.
      o #> 1
      o #> {{"a":1,"b":2}}
      o #> a
        #> b
        #> c

