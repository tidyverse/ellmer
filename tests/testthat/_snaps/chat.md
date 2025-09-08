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
      <Chat OpenAI/gpt-4.1-nano turns=3 tokens=15/5 $0.00>
      -- system [0] ------------------------------------------------------------------
      Be terse.
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

# old extract methods are deprecated

    Code
      chat_null$extract_data()
    Condition
      Warning:
      `Chat$extract_data()` was deprecated in ellmer 0.2.0.
      i Please use `Chat$chat_structured()` instead.
    Code
      chat_null$extract_data_async()
    Condition
      Warning:
      `Chat$extract_data_async()` was deprecated in ellmer 0.2.0.
      i Please use `Chat$chat_structured_async()` instead.

