# informative error if no key

    Code
      key_get("FOO")
    Condition
      Error:
      ! Can't find env var `FOO`.

# echo="output" replaces echo="text"

    Code
      expect_equal(check_echo("text"), "output")
    Condition
      Warning:
      `echo = "text"` was deprecated in ellmer 0.2.0.
      i Please use `echo = "output"` instead.

