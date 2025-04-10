# checks inputs

    Code
      interpolate(1)
    Condition
      Error in `interpolate()`:
      ! `prompt` must be a single string, not the number 1.
    Code
      interpolate("x", 1)
    Condition
      Error in `interpolate()`:
      ! All elements of `...` must be named

# has a nice print method

    Code
      interpolate("Hi!")
    Output
      [1] | Hi!

# print method truncates many elements

    Code
      print(prompt, max_items = 1)
    Output
      [1] | x
          | y
      ... and 1 more.
    Code
      print(prompt, max_lines = 2)
    Output
      [1] | x
          | y
          | ...
      ... and 1 more.
    Code
      print(prompt, max_lines = 3)
    Output
      [1] | x
          | y
      [2] | a
          | ...

