# system prompt must be a character vector

    Code
      chat_openai_test(1)
    Condition
      Error in `self$set_system_prompt()`:
      ! `value` must be a character vector or `NULL`, not the number 1.

# has a basic print method

    Code
      chat
    Output
      <Chat OpenAI/gpt-4o turns=3 tokens=15/5 $0.00>
      -- system [0] ------------------------------------------------------------------
      You're a helpful assistant that returns very minimal output
      -- user [15] -------------------------------------------------------------------
      What's 1 + 1?
      What's 1 + 2?
      -- assistant [5] ---------------------------------------------------------------
      2
      
      3

# print method shows cumulative tokens & cost

    Code
      chat
    Output
      <Chat OpenAI/gpt-4o turns=4 tokens=45000/1500 $0.13>
      -- user [15000] ----------------------------------------------------------------
      Input 1
      -- assistant [500] -------------------------------------------------------------
      Output 1
      -- user [14500] ----------------------------------------------------------------
      Input 2
      -- assistant [1000] ------------------------------------------------------------
      Output 1

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
      chat$chat("What are Joe, Hadley, Simon, and Tom's favorite colors?")
    Condition
      Warning:
      Failed to evaluate 4 tool calls.
      x [user_favorite_color (ID)]: User denied tool request
      x [user_favorite_color (ID)]: User denied tool request
      x [user_favorite_color (ID)]: User denied tool request
      i ... and 1 more.
    Output
      [1] "Cannot access favorite colors."
    Code
      chat
    Output
      <Chat OpenAI/gpt-4o-mini turns=5 tokens=287/89 $0.00>
      -- system [0] ------------------------------------------------------------------
      Be very terse, not even punctuation.
      -- user [74] -------------------------------------------------------------------
      What are Joe, Hadley, Simon, and Tom's favorite colors?
      -- assistant [82] --------------------------------------------------------------
      [tool request (ID)]: user_favorite_color(user = 
      "Joe")
      [tool request (ID)]: user_favorite_color(user = 
      "Hadley")
      [tool request (ID)]: user_favorite_color(user = 
      "Simon")
      [tool request (ID)]: user_favorite_color(user = 
      "Tom")
      -- user [57] -------------------------------------------------------------------
      [tool result  (ID)]: Error: User denied tool request
      [tool result  (ID)]: Error: User denied tool request
      [tool result  (ID)]: Error: User denied tool request
      [tool result  (ID)]: Error: User denied tool request
      -- assistant [7] ---------------------------------------------------------------
      Cannot access favorite colors.

