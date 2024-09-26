# default model is reported

    Code
      . <- new_chat_openai()$chat("Hi")
    Message
      Using model = "gpt-4o-mini".

# repeated tool calls (sync)

    Code
      chat
    Output
      <Chat messages=12>
    Message
      -- system ----------------------------------------------------------------------
      Be very terse, not even punctuation.
      -- user ------------------------------------------------------------------------
      Pick a random number. If it's positive, tell me the current time in New York.
      If it's negative, tell me the current time in Seattle. Use ISO-8601, e.g.
      '2006-01-02T15:04:05'.
      -- assistant -------------------------------------------------------------------
      Tool calls:
      rnorm(n = 1L)
      -- tool ------------------------------------------------------------------------
      1
      -- assistant -------------------------------------------------------------------
      Tool calls:
      get_time(tz = "America/New_York")
      -- tool ------------------------------------------------------------------------
      2020-08-01 14:00:00
      -- assistant -------------------------------------------------------------------
      2020-08-01T14:00:00
      -- user ------------------------------------------------------------------------
      Great. Do it again.
      -- assistant -------------------------------------------------------------------
      Tool calls:
      rnorm(n = 1L)
      -- tool ------------------------------------------------------------------------
      -1
      -- assistant -------------------------------------------------------------------
      Tool calls:
      get_time(tz = "America/Los_Angeles")
      -- tool ------------------------------------------------------------------------
      2020-08-01 11:00:00
      -- assistant -------------------------------------------------------------------
      2020-08-01T11:00:00
