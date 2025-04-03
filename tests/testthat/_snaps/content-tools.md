# $chat() echoes tool requests and results

    Code
      chat$chat("What's the current date in Y-M-D format?")
    Message
      > [tool request (ID)]: tool_001()
      v [tool result  (ID)]: 2024-01-01
    Output
      2024-01-01
    Code
      chat$chat("Ask the user to enter a password")
    Message
      > [tool request (ID)]: tool_002()
      ! [tool result  (ID)]: Error: User denied tool request
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
      > [tool request (x1)]: my_tool()
      v [tool result  (x1)]: 1
      > [tool request (x2)]: my_tool(x = 1)
      ! [tool result  (x2)]: Error: unused argument (x = 1)
      > [tool request (x3)]: tool_list()
      v [tool result  (x3)]: {"a":1,"b":2}
      > [tool request (x4)]: tool_chr()
      v [tool result  (x4)]:
        a
        b
        c
      
      > [tool request (x5)]: tool_abort()
      ! [tool result  (x5)]: Tool calling failed with error:
        Unexpected input
        i Please revise and try again.
      

# invoke_tools_async() echoes tool requests and results

    Code
      . <- sync(invoke_tools_async(turn, echo = "output"))
    Message
      > [tool request (x1)]: my_tool()
      > [tool request (x2)]: my_tool(x = 1)
      > [tool request (x3)]: tool_list()
      > [tool request (x4)]: tool_chr()
      > [tool request (x5)]: tool_abort()
      ! [tool result  (x2)]: Error: unused argument (x = 1)
      ! [tool result  (x5)]: Tool calling failed with error:
        Unexpected input
        i Please revise and try again.
      
      v [tool result  (x1)]: 1
      v [tool result  (x3)]: {"a":1,"b":2}
      v [tool result  (x4)]:
        a
        b
        c
      

