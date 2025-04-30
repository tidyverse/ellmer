# system prompt must be a character vector

    Code
      chat_openai_test(1)
    Condition
      Error in `self$set_system_prompt()`:
      ! `value` must be a character vector or `NULL`, not the number 1.

# can't chat with multiple prompts

    Code
      chat$chat(prompt)
    Condition
      Error in `chat$chat()`:
      ! `...` can only accept a single prompt.

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
      chat$register_callback("chat", function(data) NULL)
    Condition
      Error:
      ! `event` must be one of "tool_request" or "tool_result", not "chat".

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

