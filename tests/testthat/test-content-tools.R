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
  expect_equal(tool_error_string(res), "unused argument (x = 1)")
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
  expect_equal(tool_error_string(res), "unused argument (x = 1)")
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

test_that("$chat() echoes tool requests and results", {
  chat <- chat_openai_test(
    "Be very terse, not even punctuation.",
    echo = "text"
  )
  chat$register_tool(tool(function() "2024-01-01", "Return the current date"))
  chat$register_tool(tool(
    function() stop("User denied tool request"),
    "Ask user to enter a password"
  ))

  expect_snapshot(
    {
      chat$chat("What's the current date in Y-M-D format?")
      chat$chat("Ask the user to enter a password")
      chat
    },
    transform = function(value) gsub(" \\(\\w+_[a-z0-9A-Z]+\\)", " (ID)", value)
  )
})

test_that("invoke_tools() echoes tool requests and results", {
  turn <- turn_with_tool_requests()

  expect_silent(invoke_tools(turn))
  expect_snapshot(. <- invoke_tools(turn, echo = "output"))
})

test_that("invoke_tools_async() echoes tool requests and results", {
  turn <- turn_with_tool_requests()

  expect_silent(sync(invoke_tools_async(turn)))
  expect_snapshot(. <- sync(invoke_tools_async(turn, echo = "output")))
})
