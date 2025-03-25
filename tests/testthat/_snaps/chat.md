# has a basic print method

    Code
      chat
    Output
      <Chat turns=3 tokens=15/5>
      -- system [0] ------------------------------------------------------------------
      You're a helpful assistant that returns very minimal output
      -- user [15] -------------------------------------------------------------------
      What's 1 + 1?
      What's 1 + 2?
      -- assistant [5] ---------------------------------------------------------------
      2
      
      3

# chat can get and register a list of tools

    Code
      chat$register_tools(tools[[1]])
    Condition
      Error in `chat$register_tools()`:
      ! `tools` must be a list of tools created with `ellmer::tool()`.
      i Did you mean to call `$register_tool()`?

