test_that("tool can get name", {
  f <- function() {}
  td <- tool(f, "")
  expect_equal(td@name, "f")

  td <- tool(
    function() {},
    ""
  )
  expect_match(td@name, "^tool_")
})

test_that("json_schema_parameters generates correct paramters if no arguments", {
  expect_equal(
    as_json(test_provider(), type_object()),
    list(
      type = "object",
      description = "",
      properties = set_names(list()),
      required = list(),
      additionalProperties = FALSE
    )
  )
})

# tool_annotations() -------------------------------------------------------

test_that("tool_annotations(): NULL values are stripped", {
  expect_equal(tool_annotations(), set_names(list()))
})

test_that("tool_annotations(): checks its inputs", {
  expect_snapshot(error = TRUE, {
    tool_annotations(title = list("Something unexpected"))
    tool_annotations(read_only_hint = "yes")
    tool_annotations(open_world_hint = "yes")
    tool_annotations(idempotent_hint = "no")
    tool_annotations(destructive_hint = "no")
  })
})

test_that("tool_annotations(): allows additional properties", {
  expect_equal(
    tool_annotations(description = "foo"),
    list(description = "foo")
  )
})

test_that("tool() allows annotations", {
  annotations <- tool_annotations(title = "My Tool", read_only_hint = TRUE)

  # fmt: skip
  expect_equal(
    tool(function() { }, "My tool", .annotations = annotations)@annotations,
    annotations
  )
})
