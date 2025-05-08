test_that("invoke_tool returns a ContentToolResult", {
  tool <- tool(function() 1, "A tool", .name = "my_tool")

  res <- invoke_tool(
    ContentToolRequest(
      id = "x",
      name = "my_tool",
      arguments = list(),
      tool = tool
    )
  )
  expect_s3_class(res, "ellmer::ContentToolResult")
  expect_equal(res@error, NULL)
  expect_false(tool_errored(res))
  expect_equal(res@value, 1)
  expect_s3_class(res@request, "ellmer::ContentToolRequest")
  expect_equal(res@request@id, "x")
  expect_equal(res@request@tool, tool)
  expect_equal(res@request@arguments, list())

  res <- invoke_tool(
    ContentToolRequest(
      id = "x",
      name = "my_tool",
      arguments = list(x = 1),
      tool = tool
    )
  )
  expect_s3_class(res, "ellmer::ContentToolResult")
  expect_s3_class(res@error, "condition")
  expect_true(tool_errored(res))
  expect_match(tool_error_string(res), "unused argument", ignore.case = TRUE)
  expect_equal(res@value, NULL)
  expect_equal(res@extra, list())
  expect_s3_class(res@request, "ellmer::ContentToolRequest")
  expect_equal(res@request@id, "x")
  expect_equal(res@request@tool, tool)
  expect_equal(res@request@arguments, list(x = 1))

  res <- invoke_tool(
    ContentToolRequest(
      id = "x",
      arguments = list(x = 1),
      name = "my_tool"
    )
  )
  expect_s3_class(res, "ellmer::ContentToolResult")
  expect_equal(res@error, "Unknown tool")
  expect_equal(tool_error_string(res), "Unknown tool")
  expect_true(tool_errored(res))
  expect_equal(res@value, NULL)
  expect_equal(res@extra, list())
  expect_s3_class(res@request, "ellmer::ContentToolRequest")
  expect_equal(res@request@id, "x")
  expect_equal(res@request@tool, NULL)
  expect_equal(res@request@arguments, list(x = 1))

  tool_ctr <- tool(
    function() ContentToolResult(value = 1, extra = list(a = 1)),
    "A tool that returns ContentToolResult",
    .name = "my_tool"
  )
  res <- invoke_tool(
    ContentToolRequest(
      id = "x",
      name = "my_tool",
      arguments = list(),
      tool = tool_ctr
    )
  )
  expect_s3_class(res, "ellmer::ContentToolResult")
  expect_equal(res@error, NULL)
  expect_false(tool_errored(res))
  expect_equal(res@value, 1)
  expect_equal(res@extra, list(a = 1))
  expect_s3_class(res@request, "ellmer::ContentToolRequest")
  expect_equal(res@request@id, "x")
  expect_equal(res@request@tool, tool_ctr)
  expect_equal(res@request@arguments, list())
})

test_that("invoke_tool_async returns a ContentToolResult", {
  tool <- tool(function() 1, "A tool", .name = "my_tool")

  res <- sync(invoke_tool_async(
    ContentToolRequest(
      id = "x",
      name = "my_tool",
      arguments = list(),
      tool = tool
    )
  ))
  expect_s3_class(res, "ellmer::ContentToolResult")
  expect_equal(res@error, NULL)
  expect_false(tool_errored(res))
  expect_equal(res@value, 1)
  expect_s3_class(res@request, "ellmer::ContentToolRequest")
  expect_equal(res@request@id, "x")
  expect_equal(res@request@tool, tool)
  expect_equal(res@request@arguments, list())

  res <- sync(invoke_tool_async(
    ContentToolRequest(
      id = "x",
      name = "my_tool",
      arguments = list(x = 1),
      tool = tool
    )
  ))
  expect_s3_class(res, "ellmer::ContentToolResult")
  expect_s3_class(res@error, "condition")
  expect_true(tool_errored(res))
  expect_match(tool_error_string(res), "unused argument", ignore.case = TRUE)
  expect_equal(res@value, NULL)
  expect_equal(res@extra, list())
  expect_s3_class(res@request, "ellmer::ContentToolRequest")
  expect_equal(res@request@id, "x")
  expect_equal(res@request@tool, tool)
  expect_equal(res@request@arguments, list(x = 1))

  res <- sync(invoke_tool_async(
    ContentToolRequest(
      id = "x",
      arguments = list(x = 1),
      name = "my_tool"
    )
  ))
  expect_s3_class(res, "ellmer::ContentToolResult")
  expect_equal(res@error, "Unknown tool")
  expect_equal(tool_error_string(res), "Unknown tool")
  expect_true(tool_errored(res))
  expect_equal(res@value, NULL)
  expect_equal(res@extra, list())
  expect_s3_class(res@request, "ellmer::ContentToolRequest")
  expect_equal(res@request@id, "x")
  expect_equal(res@request@tool, NULL)
  expect_equal(res@request@arguments, list(x = 1))

  tool_ctr <- tool(
    function() ContentToolResult(value = 1, extra = list(a = 1)),
    "A tool that returns ContentToolResult",
    .name = "my_tool"
  )
  res <- sync(invoke_tool_async(
    ContentToolRequest(
      id = "x",
      name = "my_tool",
      arguments = list(),
      tool = tool_ctr
    )
  ))
  expect_s3_class(res, "ellmer::ContentToolResult")
  expect_equal(res@error, NULL)
  expect_false(tool_errored(res))
  expect_equal(res@value, 1)
  expect_equal(res@extra, list(a = 1))
  expect_s3_class(res@request, "ellmer::ContentToolRequest")
  expect_equal(res@request@id, "x")
  expect_equal(res@request@tool, tool_ctr)
  expect_equal(res@request@arguments, list())
})

test_that("invoke_tools() echoes tool requests and results", {
  turn <- fixture_turn_with_tool_requests()

  expect_silent(invoke_tools(turn))
  expect_snapshot(. <- invoke_tools(turn, echo = "output"))
})

test_that("invoke_tools_async() echoes tool requests and results", {
  turn <- fixture_turn_with_tool_requests()

  expect_silent(sync(invoke_tools_async(turn)))
  expect_snapshot(. <- sync(invoke_tools_async(turn, echo = "output")))
})

test_that("invoke_tools() converts to R data structures", {
  out <- NULL
  tool <- tool(
    function(...) out <<- list(...),
    "A tool",
    x = type_array(items = type_number()),
    y = type_array(items = type_string())
  )

  res <- invoke_tool(
    ContentToolRequest(
      id = "x",
      name = "my_tool",
      arguments = list(x = list(1, 2), y = list()),
      tool = tool
    )
  )
  expect_equal(out$x, c(1, 2))
  expect_equal(out$y, character())
})

test_that("invoke_tools_async() converts to R data structures", {
  out <- NULL
  tool <- tool(
    function(...) out <<- list(...),
    "A tool",
    x = type_array(items = type_number()),
    y = type_array(items = type_string())
  )

  res <- sync(invoke_tool_async(
    ContentToolRequest(
      id = "x",
      name = "my_tool",
      arguments = list(x = list(1, 2), y = list()),
      tool = tool
    )
  ))
  expect_equal(out$x, c(1, 2))
  expect_equal(out$y, character())
})

test_that("tool error warnings", {
  errors <- list(
    ContentToolResult(
      error = "The JSON was invalid: {[1, 2, 3]}",
      request = ContentToolRequest(
        id = "call1",
        name = "returns_json"
      )
    ),
    ContentToolResult(
      error = rlang::catch_cnd(stop("went boom!")),
      request = ContentToolRequest(
        id = "call2",
        name = "throws"
      )
    )
  )

  expect_snapshot(
    warn_tool_errors(errors)
  )
})

test_that("match_tools() matches tools in a turn to a list of tools", {
  turn_single <- Turn(
    "assistant",
    list(ContentToolRequest("y1", "unknown", list()))
  )
  expect_null(turn_single@contents[[1]]@tool)
  expect_s7_class(match_tools(turn_single, list()), Turn)
  # unmatched requests have NULL tool
  expect_null(match_tools(turn_single, list())@contents[[1]]@tool)

  tools <- fixture_list_of_tools()
  turn <- fixture_turn_with_tool_requests(with_tool = FALSE)

  turn_matched <- match_tools(turn, tools)
  expect_equal(turn_matched, fixture_turn_with_tool_requests(with_tool = TRUE))
})
