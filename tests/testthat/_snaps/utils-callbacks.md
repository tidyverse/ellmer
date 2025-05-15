# CallbackManager catches argument mismatches

    Code
      callbacks$add("foo")
    Condition
      Error:
      ! `callback` must be a function, not the string "foo".
    Code
      callbacks$add(function(foo) NULL)
    Condition
      Error:
      ! `callback` must have the argument `data`; it currently has `foo`.
    Code
      callbacks$add(function(x, y) x + y)
    Condition
      Error:
      ! `callback` must have the argument `data`; it currently has `x` and `y`.

---

    Code
      callbacks$invoke()
    Condition
      Error in `private$callbacks[[as.character(id)]]()`:
      ! argument "data" is missing, with no default
    Code
      callbacks$invoke(1, 2)
    Condition
      Error in `private$callbacks[[as.character(id)]]()`:
      ! unused argument (2)

