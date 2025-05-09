# normalize_turns throws useful errors

    Code
      normalize_turns(1)
    Condition
      Error:
      ! `turns` must be an unnamed list or `NULL`, not the number 1.
    Code
      normalize_turns(list(1))
    Condition
      Error in `normalize_turns()`:
      ! Every element of `turns` must be a `turn`.
    Code
      normalize_turns(list(sys_msg, user_msg), 1)
    Condition
      Error in `normalize_turns()`:
      ! `system_prompt` must be a single string or `NULL`, not the number 1.
    Code
      normalize_turns(list(sys_msg, user_msg), "foo2")
    Condition
      Error:
      ! `system_prompt` and `turns[[1]]` can't contain conflicting system prompts.

# as_user_turn gives useful errors

    Code
      as_user_turn(list())
    Condition
      Error:
      ! `...` must contain at least one input.
    Code
      as_user_turn(list(x = 1))
    Condition
      Error:
      ! `...` must be unnamed.
    Code
      as_user_turn(1)
    Condition
      Error in `FUN()`:
      ! `...` must be made up strings or <content> objects, not the number 1.

# as_user_turns gives useful errors

    Code
      as_user_turns(1)
    Condition
      Error:
      ! `1` must be a list or prompt, not the number 1.
    Code
      as_user_turns(x)
    Condition
      Error in `FUN()`:
      ! `x[[1]]` must be made up strings or <content> objects, not the number 1.

# turns have a reasonable print method

    Code
      Turn("user", "hello")
    Output
      <Turn: user>
      hello

