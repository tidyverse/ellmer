# get_rounds() aborts on a leading tool-result turn

    Code
      get_rounds(turns)
    Condition
      Error in `get_rounds()`:
      ! Found a tool-result turn with no preceding user turn to start a round.

# Round validator rejects a tool-result turn as input

    Code
      Round(input = fixture_tool_result_turn(), response = list())
    Condition
      Error:
      ! <ellmer::Round> object is invalid:
      - `input` must be a user turn that is not a tool result.

