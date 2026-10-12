# Round prints input and response turns with role headers

    Code
      print(round)
    Output
      <Round>
      <Turn: system>
      Be nice
      <Turn: user>
      Hi
      <Turn: assistant>
      Hello

# contents_*() label each turn by role

    Code
      cat(contents_text(round))
    Output
      <system>
      Be nice
      </system>
      
      <user>
      Hi
      </user>
      
      <assistant>
      Hello
      </assistant>

---

    Code
      cat(contents_markdown(round))
    Output
      ## System
      
      Be nice
      
      ## User
      
      Hi
      
      ## Assistant
      
      Hello

---

    Code
      cat(contents_html(round))
    Output
      <h2>System</h2>
      <p>Be nice</p>
      
      <h2>User</h2>
      <p>Hi</p>
      
      <h2>Assistant</h2>
      <p>Hello</p>

# get_rounds() aborts on a leading tool-result turn

    Code
      get_rounds(turns)
    Condition
      Error in `get_rounds()`:
      ! Found a response turn with no preceding input turn to start a round.

# Round validator rejects a tool-result turn as input

    Code
      Round(input = list(fixture_tool_result_turn()), response = list())
    Condition
      Error:
      ! <ellmer::Round> object is invalid:
      - `input` must not contain tool-result turns.

