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

