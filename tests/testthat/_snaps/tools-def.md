# tools have a print method

    Code
      f
    Output
      # <ellmer::ToolDef> my_fun(x, y)
      # @name: my_fun
      # @description: a simple function
      # @convert: TRUE
      #
      function(x = 1, y = 2) {
          x + y
        }

# old arguments are deprecated

    Code
      f <- tool(function(x) x * 2, .name = "double", .description = "double the input",
      x = type_number(), .convert = FALSE, .annotations = tool_annotations(title = "My Tool"))
    Condition
      Warning:
      The `...` argument of `tool()` is deprecated as of ellmer 0.3.0.
      i Please use the `arguments` argument instead.
      Warning:
      The `.name` argument of `tool()` is deprecated as of ellmer 0.3.0.
      i Please use the `name` argument instead.
      Warning:
      The `.description` argument of `tool()` is deprecated as of ellmer 0.3.0.
      i Please use the `description` argument instead.
      Warning:
      The `.convert` argument of `tool()` is deprecated as of ellmer 0.3.0.
      i Please use the `convert` argument instead.
      Warning:
      The `.annotations` argument of `tool()` is deprecated as of ellmer 0.3.0.
      i Please use the `annotations` argument instead.

# checks its arguments

    Code
      tool(1)
    Condition
      Error in `tool()`:
      ! `fun` must be a function, not the number 1.
    Code
      tool(identity, 1)
    Condition
      Error in `tool()`:
      ! `description` must be a single string, not the number 1.
    Code
      tool(identity, "", name = 1)
    Condition
      Error in `tool()`:
      ! `name` must be a single string or `NULL`, not the number 1.
    Code
      tool(identity, "", name = "...")
    Condition
      Error in `tool()`:
      ! `name` must contain only letters, numbers, - and _.
    Code
      tool(identity, "", arguments = 1)
    Condition
      Error in `tool()`:
      ! `arguments` must be a named list, not the number 1.
    Code
      tool(identity, "", convert = 1)
    Condition
      Error in `tool()`:
      ! `convert` must be `TRUE` or `FALSE`, not the number 1.

# arguments must match function formals

    Code
      tool(fun, "", arguments = list(z = type_number()))
    Condition
      Error in `tool()`:
      ! Names of `arguments` must match formals of `fun`
      * Extra type definitions: "z"
      * Missing type definitions: "x" and "y"
    Code
      tool(fun, "", arguments = list(x = type_number(), y = 1))
    Condition
      Error in `tool()`:
      ! `arguments$y` must be a <Type>, not the number 1.

# can check tool/tools

    Code
      check_tool(1)
    Condition
      Error:
      ! `1` must be a <ToolDef>, not the number 1.
    Code
      check_tools(1)
    Condition
      Error:
      ! `1` must be a list, not the number 1.
    Code
      check_tools(x)
    Condition
      Error:
      ! `x[[1]]` must be a <ToolDef>, not the number 1.

# tool_annotations(): checks its inputs

    Code
      tool_annotations(title = list("Something unexpected"))
    Condition
      Error in `tool_annotations()`:
      ! `title` must be a character vector or `NULL`, not a list.
    Code
      tool_annotations(read_only_hint = "yes")
    Condition
      Error in `tool_annotations()`:
      ! `read_only_hint` must be `TRUE`, `FALSE`, or `NULL`, not the string "yes".
    Code
      tool_annotations(open_world_hint = "yes")
    Condition
      Error in `tool_annotations()`:
      ! `open_world_hint` must be `TRUE`, `FALSE`, or `NULL`, not the string "yes".
    Code
      tool_annotations(idempotent_hint = "no")
    Condition
      Error in `tool_annotations()`:
      ! `idempotent_hint` must be `TRUE`, `FALSE`, or `NULL`, not the string "no".
    Code
      tool_annotations(destructive_hint = "no")
    Condition
      Error in `tool_annotations()`:
      ! `destructive_hint` must be `TRUE`, `FALSE`, or `NULL`, not the string "no".

