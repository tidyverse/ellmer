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
    Code
      print(ellmer_prompt("a\nb\nc\nd\ne"), max_lines = 3)
    Output
      [1] | a
          | b
          | c
          | ...

# errors if the path does not exist

    Code
      interpolate_file("does-not-exist.md", x = 1)
    Condition
      Error in `interpolate_file()`:
      ! `path` 'does-not-exist.md' does not exist.
    Code
      interpolate_package("ellmer", "does-not-exist.md", x = 1)
    Condition
      Error in `interpolate_package()`:
      ! ellmer does not have "does-not-exist.md" in its prompts/ directory.
      i Run `dir(system.file("prompts", package = "ellmer"))` to see available prompts.

