# prop_whole_number validates inputs

    Code
      check_prop()("x")
    Condition
      Error:
      ! <class> object properties are invalid:
      - @prop must be <double>, not <character>
    Code
      check_prop()(c(1, 2))
    Condition
      Error:
      ! <class> object properties are invalid:
      - @prop must be a whole number, not a double vector.
    Code
      check_prop()(1.5)
    Condition
      Error:
      ! <class> object properties are invalid:
      - @prop must be a whole number, not the number 1.5.
    Code
      check_prop(min = 1)(0)
    Condition
      Error:
      ! <class> object properties are invalid:
      - @prop must be at least 1, not 0.
    Code
      check_prop(max = -1)(0)
    Condition
      Error:
      ! <class> object properties are invalid:
      - @prop must be at most -1, not 0.
    Code
      check_prop()(NA_real_)
    Condition
      Error:
      ! <class> object properties are invalid:
      - @prop must not be missing.

# prop_whole_decimal validates inputs

    Code
      check_prop()("x")
    Condition
      Error:
      ! <class> object properties are invalid:
      - @prop must be <double>, not <character>
    Code
      check_prop()(c(1, 2))
    Condition
      Error:
      ! <class> object properties are invalid:
      - @prop must be a whole number, not a double vector.
    Code
      check_prop()(1.5)
    Output
      <class>
       @ prop: num 1.5
    Code
      check_prop(min = 1)(0)
    Condition
      Error:
      ! <class> object properties are invalid:
      - @prop must be at least 1, not 0.
    Code
      check_prop(max = -1)(0)
    Condition
      Error:
      ! <class> object properties are invalid:
      - @prop must be at most -1, not 0.
    Code
      check_prop(max = -1)(Inf)
    Condition
      Error:
      ! <class> object properties are invalid:
      - @prop must be finite, not Inf.
    Code
      check_prop()(NA_real_)
    Condition
      Error:
      ! <class> object properties are invalid:
      - @prop must not be missing.

