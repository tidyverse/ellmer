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
      <Chat OpenAI/gpt-4.1-nano turns=3 input=10+5 output=5>
      -- system ----------------------------------------------------------------------
      Be terse.
      -- user ------------------------------------------------------------------------
      What's 1 + 1?
      What's 1 + 2?
      -- assistant [input=10+5 output=5] ---------------------------------------------
      2
      
      3

# print method shows interrupted for partial turns

    Code
      chat
    Output
      <Chat OpenAI/gpt-4o turns=4 input=15000 output=500 cost=$0.20>
      -- user ------------------------------------------------------------------------
      Input 1
      -- assistant [input=15000 output=500 cost=$0.20] -------------------------------
      Output 1
      -- user ------------------------------------------------------------------------
      Input 2
      -- assistant [interrupted] -----------------------------------------------------
      Partial output...

# print method shows custom reason for partial turns

    Code
      chat
    Output
      <Chat OpenAI/gpt-4o turns=2 input=0 output=0 cost=$0.00>
      -- user ------------------------------------------------------------------------
      Input 1
      -- assistant [cancelled] -------------------------------------------------------
      Partial output...

# print method shows cumulative tokens & cost

    Code
      chat
    Output
      <Chat OpenAI/gpt-4o turns=4 input=45000 output=1500 cost=$0.30>
      -- user ------------------------------------------------------------------------
      Input 1
      -- assistant [input=15000 output=500 cost=$0.20] -------------------------------
      Output 1
      -- user ------------------------------------------------------------------------
      Input 2
      -- assistant [input=30000 output=1000 cost=$0.10] ------------------------------
      Output 1

# can compute costs

    Code
      details
    Output
      # A tibble: 2 x 5
        input output cached_input cost       input_preview
        <dbl>  <dbl>        <dbl> <ellmr_dl> <chr>        
      1 15000    500            0 $0.20      Text[Input 1]
      2 30000   1000            0 $0.10      Text[Input 2]

# stream() rejects non-controller object

    Code
      coro::collect(chat$stream("hi", controller = TRUE))
    Condition
      Error in `chat$stream()`:
      ! `controller` must be an <ellmer_stream_controller> object created by `stream_controller()`.

# stream_async() rejects non-controller object

    Code
      sync(coro::async_collect(chat$stream_async("hi", controller = list())))
    Condition
      Error in `chat$stream_async()`:
      ! `controller` must be an <ellmer_stream_controller> object created by `stream_controller()`.

