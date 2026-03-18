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

test_that("built-in tool constructors set description and annotations", {
  openai_ws <- openai_tool_web_search()
  expect_match(openai_ws@description, "Search")
  expect_equal(openai_ws@annotations$title, "Web search")

  claude_ws <- claude_tool_web_search()
  expect_match(claude_ws@description, "Search")
  expect_equal(claude_ws@annotations$title, "Web search")

  claude_wf <- claude_tool_web_fetch()
  expect_match(claude_wf@description, "Fetch")
  expect_equal(claude_wf@annotations$title, "Web fetch")

  google_ws <- google_tool_web_search()
  expect_match(google_ws@description, "Search")
  expect_equal(google_ws@annotations$title, "Web search")

  google_wf <- google_tool_web_fetch()
  expect_match(google_wf@description, "Fetch")
  expect_equal(google_wf@annotations$title, "Web fetch")
})
