test_that("ToolBuiltIn has description and annotations", {
  tool <- ToolBuiltIn(
    name = "test_tool",
    description = "A test tool.",
    annotations = tool_annotations(title = "Test Tool", read_only_hint = TRUE),
    json = list(type = "test")
  )

  expect_equal(tool@name, "test_tool")
  expect_equal(tool@description, "A test tool.")
  expect_equal(tool@annotations$title, "Test Tool")
  expect_true(tool@annotations$read_only_hint)
  expect_equal(tool@json, list(type = "test"))
})

test_that("ToolBuiltIn defaults for description and annotations", {
  tool <- ToolBuiltIn(name = "minimal", json = list())

  expect_equal(tool@description, "")
  expect_equal(tool@annotations, list())
})

test_that("built-in tools", {
  get_built_in_tools <- function() {
    exports <- getNamespaceExports("ellmer")
    tool_fns <- exports[grepl("_tool_", exports)]

    tools <- list()
    for (fn_name in tool_fns) {
      fn <- getExportedValue("ellmer", fn_name)
      result <- tryCatch(fn(), error = function(e) NULL)
      if (!is.null(result) && S7_inherits(result, ToolBuiltIn)) {
        tools[[fn_name]] <- result
      }
    }
    tools
  }

  expect_gte(length(get_built_in_tools()), 1)

  built_in_tools <- get_built_in_tools()

  for (fn_name in names(built_in_tools)) {
    test_that(paste0(fn_name, "() sets description and annotations"), {
      tool <- built_in_tools[[fn_name]]
      expect_match(tool@description, ".")
      expect_match(tool@annotations$title, ".")
    })
  }
})
