test_that("can call tools directory", {
  f <- tool(function() 1, description = "a simple function")
  expect_equal(f(), 1)
})

test_that("tools have a print method", {
  fun <- function(x = 1, y = 2) {
    x + y
  }
  environment(fun) <- globalenv()

  f <- tool(
    fun,
    name = "my_fun",
    arguments = list(x = type_string(), y = type_number()),
    description = "a simple function"
  )
  expect_snapshot(f)
})

test_that("tool can get name", {
  f <- function() {}
  td <- tool(f, description = "")
  expect_equal(td@name, "f")

  td <- tool(
    function() {},
    description = ""
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

test_that("old arguments are deprecated", {
  expect_snapshot({
    f <- tool(
      function(x) x * 2,
      .name = "double",
      .description = "double the input",
      x = type_number(),
      .convert = FALSE,
      .annotations = tool_annotations(title = "My Tool")
    )
  })

  expect_equal(f@name, "double")
  expect_equal(f@convert, FALSE)
  expect_equal(f@description, "double the input")
  expect_equal(f@arguments, TypeObject(properties = list(x = type_number())))
  expect_equal(f@annotations, tool_annotations(title = "My Tool"))
})

test_that("checks its arguments", {
  expect_snapshot(error = TRUE, {
    tool(1)
    tool(identity, 1)
    tool(identity, "", name = 1)
    tool(identity, "", name = "...")
    tool(identity, "", arguments = 1)
    tool(identity, "", convert = 1)
  })
})

test_that("arguments must match function formals", {
  fun <- function(x, y) {}

  expect_snapshot(error = TRUE, {
    tool(fun, "", arguments = list(z = type_number()))
    tool(fun, "", arguments = list(x = type_number(), y = 1))
  })
})

test_that("can check tool/tools", {
  x <- list(1)
  expect_snapshot(error = TRUE, {
    check_tool(1)
    check_tools(1)
    check_tools(x)
  })
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
  tool_def <- tool(function() {}, description = "", annotations = annotations)
  expect_equal(tool_def@annotations, annotations)
})
