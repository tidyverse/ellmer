# CallbackManager catches argument mismatches

    Code
      callbacks$add("foo")
    Condition
      Error in `callbacks$add()`:
      ! `callback` must be a function.
    Code
      callbacks$add(function(x, y) x + y)
    Condition
      Error:
      ! Only the first argument of `callback` can be required.
    Code
      callbacks$add(function(x = 1, y) x + y)
    Condition
      Error:
      ! Only the first argument of `callback` can be required.

