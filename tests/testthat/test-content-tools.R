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

  expect_silent(coro::collect(invoke_tools(turn)))
  expect_snapshot(. <- coro::collect(invoke_tools(turn, echo = "output")))
})

test_that("invoke_tools() yields tool requests and results", {
  turn <- fixture_turn_with_tool_requests()

  steps <- coro::collect(invoke_tools(turn, yield_request = TRUE))
  for (i in seq_along(steps)) {
    # Threaded requests and results
    if (i %% 2 == 1) {
      expect_s3_class(steps[[i]], "ellmer::ContentToolRequest")
    } else {
      expect_s3_class(steps[[i]], "ellmer::ContentToolResult")
      # results are paired with previous request
      expect_equal(steps[[i]]@request, steps[[i - 1]])
    }
  }
})

test_that("invoke_tools_async() echoes tool requests and results", {
  turn <- fixture_turn_with_tool_requests()

  expect_silent(sync({
    # Concurrent tool calls
    gen_async_promise_all(invoke_tools_async(turn))
    # Sequential tool calls
    coro::async_collect(invoke_tools_async(turn))
  }))
  expect_snapshot({
    # Concurrent tool calls
    . <- sync(gen_async_promise_all(invoke_tools_async(turn, echo = "output")))
    # Sequential tool calls
    . <- sync(coro::async_collect(invoke_tools_async(turn, echo = "output")))
  })
})

test_that("invoke_tools_async() yields tool requests and promises results", {
  turn <- fixture_turn_with_tool_requests()

  steps <- coro::collect(invoke_tools_async(turn, yield_request = TRUE))

  for (i in seq_along(steps)) {
    # Threaded requests and promise for results
    if (i %% 2 == 1) {
      expect_s3_class(steps[[i]], "ellmer::ContentToolRequest")
    } else {
      expect_s3_class(steps[[i]], "promise")
      result <- sync(steps[[i]])
      expect_s3_class(result, "ellmer::ContentToolResult")
      # results are paired with previous request
      expect_equal(result@request, steps[[i - 1]])
    }
  }
})

test_that("invoke_tools() converts to R data structures", {
  out <- NULL
  tool <- tool(
    function(...) out <<- list(...),
    "A tool",
    x = type_array(items = type_number()),
    y = type_array(items = type_string())
  )

  req <-
    ContentToolRequest(
      id = "x",
      name = "my_tool",
      arguments = list(x = list(1, 2), y = list()),
      tool = tool
    )

  args <- tool_request_args(req)
  expect_equal(args$x, c(1, 2))
  expect_equal(args$y, character())

  res <- invoke_tool(req)
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

  req <-
    ContentToolRequest(
      id = "x",
      name = "my_tool",
      arguments = list(x = list(1, 2), y = list()),
      tool = tool
    )

  args <- tool_request_args(req)
  expect_equal(args$x, c(1, 2))
  expect_equal(args$y, character())

  res <- sync(invoke_tool_async(req))
  expect_equal(out$x, c(1, 2))
  expect_equal(out$y, character())
})

test_that("invoke_tools() can invoke tools with args with default values", {
  out <- NULL
  tool <- tool(
    function(x, y, z = "z") out <<- list(x = x, y = y, z = z),
    "A tool",
    x = type_array(items = type_number()),
    y = type_array(items = type_string()),
    z = type_array(items = type_string(), required = FALSE)
  )

  req <- ContentToolRequest(
    id = "x",
    name = "my_tool",
    arguments = list(x = list(1, 2), y = NULL, z = NULL),
    tool = tool
  )

  args <- tool_request_args(req)
  expect_equal(args$x, c(1, 2))
  expect_equal(args$y, character()) # Required arg
  expect_equal(args$z, NULL) # Optional arg

  res <- invoke_tool(req)
  expect_equal(out$x, c(1, 2))
  expect_equal(out$y, character())
  expect_equal(out$z, "z")
})

test_that("invoke_tools_async() can invoke tools with args with default values", {
  out <- NULL
  tool <- tool(
    function(x, y, z = "z") out <<- list(x = x, y = y, z = z),
    "A tool",
    x = type_array(items = type_number()),
    y = type_array(items = type_string()),
    z = type_array(items = type_string(), required = FALSE)
  )

  req <- ContentToolRequest(
    id = "x",
    name = "my_tool",
    arguments = list(x = list(1, 2), y = NULL, z = NULL),
    tool = tool
  )

  args <- tool_request_args(req)
  expect_equal(args$x, c(1, 2))
  expect_equal(args$y, character()) # Required arg
  expect_equal(args$z, NULL) # Optional arg

  res <- sync(invoke_tool_async(req))
  expect_equal(out$x, c(1, 2))
  expect_equal(out$y, character())
  expect_equal(out$z, "z")
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
