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

