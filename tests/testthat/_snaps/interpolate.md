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

# can interpolate from a file

    Code
      interpolate_file("does-not-exist.md", x = 1)
    Condition
      Error in `interpolate_file()`:
      ! `path` 'does-not-exist.md' does not exist.

# informative errors if can't find prompts

    Code
      interpolate_package("x", "does-not-exist.md")
    Condition
      Error in `interpolate_package()`:
      ! x does not have a prompts/ directory.

---

    Code
      interpolate_package("x", "does-not-exist.md")
    Condition
      Error in `interpolate_package()`:
      ! x does not have "does-not-exist.md" in its prompts/ directory.
      i Run `dir(system.file("prompts", package = "x"))` to see available prompts.

# checks its inputs

    Code
      interpolate_package(1)
    Condition
      Error in `interpolate_package()`:
      ! `package` must be a single string, not the number 1.
    Code
      interpolate_package("x", 1)
    Condition
      Error in `interpolate_package()`:
      ! `path` must be a single string, not the number 1.

