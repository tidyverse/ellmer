# call tool gives useful error

    Code
      call_tool("foo", c(1, 2, 3))
    Output
      [1] "Error calling tool: second argument must be a list"

# repeated tool calls (sync)

    Code
      chat
    Output
      <ChatOpenAI>
      -- system ----------------------------------------------------------------------
      Be very terse, not even punctuation.
      -- user ------------------------------------------------------------------------
      Pick a random number. If it's positive, tell me the current time in New York. If it's negative, tell me the current time in Seattle. Use ISO-8601.
      -- assistant -------------------------------------------------------------------
      
      -- tool ------------------------------------------------------------------------
      1
      -- assistant -------------------------------------------------------------------
      
      -- tool ------------------------------------------------------------------------
      2020-08-01 14:00:00
      -- assistant -------------------------------------------------------------------
      2020-08-01T14:00:00
      -- user ------------------------------------------------------------------------
      Great. Do it again.
      -- assistant -------------------------------------------------------------------
      
      -- tool ------------------------------------------------------------------------
      -1
      -- assistant -------------------------------------------------------------------
      
      -- tool ------------------------------------------------------------------------
      2020-08-01 11:00:00
      -- assistant -------------------------------------------------------------------
      2020-08-01T11:00:00

# repeated tool calls (async)

    Code
      chat_async
    Output
      <ChatOpenAI>
      -- system ----------------------------------------------------------------------
      Be very terse, not even punctuation.
      -- user ------------------------------------------------------------------------
      Pick a random number. If it's positive, tell me the current time in New York. If it's negative, tell me the current time in Seattle. Use ISO-8601.
      -- assistant -------------------------------------------------------------------
      
      -- tool ------------------------------------------------------------------------
      1
      -- assistant -------------------------------------------------------------------
      
      -- tool ------------------------------------------------------------------------
      2020-08-01 14:00:00
      -- assistant -------------------------------------------------------------------
      2020-08-01T14:00:00
      -- user ------------------------------------------------------------------------
      Great. Do it again.
      -- assistant -------------------------------------------------------------------
      
      -- tool ------------------------------------------------------------------------
      -1
      -- assistant -------------------------------------------------------------------
      
      -- tool ------------------------------------------------------------------------
      2020-08-01 11:00:00
      -- assistant -------------------------------------------------------------------
      2020-08-01T11:00:00
