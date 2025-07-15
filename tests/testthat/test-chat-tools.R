# Registry ---------------------------------------------------------------------

test_that("can only set/register tools", {
  chat <- chat_openai_test()
  tool_def <- tool(function() 1, "tool")

  expect_snapshot(error = TRUE, {
    chat$register_tools(list(tool_def, 1))
    chat$set_tools(list(tool_def, 1))
  })
})

test_that("chat can get and register a list of tools", {
  chat <- chat_openai_test()
  chat2 <- chat_openai_test()

  tools <- list(
    "sys_time" = tool(
      function() strftime(Sys.time(), "%F %T"),
      name = "sys_time",
      description = "Get the current system time"
    ),
    "r_version" = tool(
      function() R.version.string,
      name = "r_version",
      description = "Get the R version of the current session"
    )
  )

  chat$register_tools(tools)
  chat2$set_tools(tools)

  expect_equal(chat$get_tools(), tools)
  expect_equal(chat2$get_tools(), chat$get_tools())

  # action = "replace" overwrites existing tools
  tool_r_major <- tool(
    function() R.version$major,
    name = "r_version_major",
    description = "Get the major version of R"
  )
  new_tools <- list("r_version_major" = tool_r_major)
  chat$set_tools(new_tools)
  expect_equal(chat$get_tools(), new_tools)
})

# Execution --------------------------------------------------------------------

test_that("can handle parallel tools", {
  vcr::local_cassette("chat-tools-parallel")

  chat <- chat_openai_test("Be terse")
  chat$register_tool(tool(
    replay(c(2, 5)),
    name = "dice",
    description = "Rolls a six-sided die"
  ))
  result <- chat$chat("Roll two dice and compute the total.")
  expect_match(result, "7")
  expect_equal(
    content_types(chat$get_turns()),
    list(
      "ContentText",
      c("ContentToolRequest", "ContentToolRequest"),
      c("ContentToolResult", "ContentToolResult"),
      "ContentText"
    )
  )
})

test_that("can handle sequential tools", {
  vcr::local_cassette("chat-tools-sequential")

  chat <- chat_openai_test("Be terse")
  chat$register_tool(tool(
    function() 1,
    name = "dice",
    description = "Rolls a dice"
  ))
  chat$register_tool(tool(
    function(roll) "Pants",
    name = "clothes",
    description = "Pick clothes to wear based on a dice roll",
    arguments = list(roll = type_number())
  ))

  result <- chat$chat(
    "Which clothes should I wear today? Roll a dice to decide."
  )
  expect_match(result, "pants", ignore.case = TRUE)
  expect_equal(
    content_types(chat$get_turns()),
    list(
      "ContentText",
      "ContentToolRequest",
      "ContentToolResult",
      "ContentToolRequest",
      "ContentToolResult",
      "ContentText"
    )
  )
})

test_that("chat warns on tool failures", {
  vcr::local_cassette("chat-tools-failure")
  chat <- chat_openai_test()

  chat$register_tool(tool(
    function(user) stop("User denied tool request"),
    name = "user_favorite_color",
    description = "Find out a user's favorite color",
    arguments = list(user = type_string("User's name"))
  ))

  expect_snapshot(
    . <- chat$chat("What are Joe, Hadley, Simon, and Tom's favorite colors?"),
    transform = function(value) gsub(" \\(\\w+_[a-z0-9A-Z]+\\)", " (ID)", value)
  )
})

test_that("tool calls can be rejected via `tool_request` callbacks", {
  vcr::local_cassette("chat-tools-reject-callback")
  chat <- chat_openai_test()

  chat$register_tool(tool(
    function(user) "red",
    name = "user_favorite_color",
    description = "Find out a user's favorite color",
    arguments = list(user = type_string("User's name"))
  ))

  chat$on_tool_request(function(request) {
    if (request@arguments$user == "Joe") {
      tool_reject("Joe denied the request.")
    }
  })

  expect_warning(
    result <- chat$chat(
      "What are Joe and Hadley's favorite colors?",
      "Write 'Joe ____ Hadley ____'. Use 'unknown' if you don't know."
    ),
    class = "ellmer_tool_failure"
  )
  expect_equal(result, ellmer_output("Joe unknown Hadley red"))
})

test_that("tool calls can be rejected via the tool function", {
  vcr::local_cassette("chat-tools-reject-tool-function")
  chat <- chat_openai_test()

  chat$register_tool(tool(
    function(user) if (user == "Joe") tool_reject() else "red",
    name = "user_favorite_color",
    description = "Find out a user's favorite color",
    arguments = list(user = type_string("User's name"))
  ))

  expect_warning(
    result <- chat$chat(
      "What are Joe and Hadley's favorite colors?",
      "Write 'Joe ____ Hadley ____'. Use 'unknown' if you don't know."
    ),
    class = "ellmer_tool_failure"
  )
  expect_equal(result, ellmer_output("Joe unknown Hadley red"))
})

# Async ------------------------------------------------------------------------

test_that("can use async tools", {
  chat <- chat_openai_test("Be very terse, not even punctuation.")
  chat$register_tool(tool(
    coro::async(function() "2024-01-01"),
    name = "current_date",
    description = "Return the current date"
  ))

  result <- sync(chat$chat_async("What's the current date in Y-M-D format?"))
  expect_match(result, "2024-01-01")

  # Can't use async tools in sync context
  expect_error(chat$chat("Great. Do it again."), class = "tool_async_error")
})

test_that("chat callbacks for tool requests/results", {
  vcr::local_cassette("chat-tools-callbacks")
  chat <- chat_openai_test()

  test_tool <- tool(
    function(user) c("red", "blue")[nchar(user) %% 2 + 1],
    name = "user_favorite_color",
    description = "Find out a user's favorite color",
    arguments = list(user = type_string("User's name"))
  )

  chat$register_tool(test_tool)

  last_request <- NULL
  cb_count_request <- 0
  cb_count_result <- 0

  chat$on_tool_request(function(request) {
    cb_count_request <<- cb_count_request + 1
    cli::cli_inform(
      "[{cb_count_request}] Tool request: {request@arguments$user}"
    )

    expect_s7_class(request, ContentToolRequest)
    expect_equal(request@tool, test_tool)
    last_request <<- request
  })
  chat$on_tool_result(function(result) {
    cb_count_result <<- cb_count_result + 1
    cli::cli_inform("[{cb_count_result}] Tool result: {result@value}")

    expect_s7_class(result, ContentToolResult)
    expect_equal(result@request, last_request)
  })

  expect_snapshot(
    . <- chat$chat("What are Joe and Hadley's favorite colors?")
  )
  expect_equal(cb_count_request, 2L)
  expect_equal(cb_count_result, 2L)

  expect_snapshot(error = TRUE, {
    chat$on_tool_request(function(data) NULL)
    chat$on_tool_result(function(data) NULL)
  })
})

test_that("$chat_async() can run tools concurrently", {
  res <- list()

  chat <- chat_openai_test()
  chat$register_tool(tool(
    coro::async(function(i) {
      res[[i]] <<- list(start = Sys.time())
      coro::await(coro::async_sleep(0.5))
      res[[i]]$end <<- Sys.time()
      i
    }),
    name = "test_async_tool",
    description = "Tests async tool usage",
    arguments = list(i = type_string("ID of the tool call"))
  ))

  sync(chat$chat_async(
    "Run `test_async_tool` twice with inputs '1' and '2'.",
    tool_mode = "concurrent"
  ))

  # The calls overlap, and both start before the first ends
  expect_true(res[[1]]$start < res[[1]]$end)
  expect_true(res[[2]]$start < res[[1]]$end)
  expect_true(res[[2]]$start < res[[2]]$end)
})

test_that("$chat_async() can run tools sequentially", {
  res <- list()

  chat <- chat_openai_test()
  chat$register_tool(tool(
    coro::async(function(i) {
      res[[i]] <<- list(start = Sys.time())
      coro::await(coro::async_sleep(0.5))
      res[[i]]$end <<- Sys.time()
      i
    }),
    name = "test_async_tool",
    description = "Tests async tool usage",
    arguments = list(i = type_string("ID of the tool call"))
  ))

  sync(chat$chat_async(
    "Run `test_async_tool` twice with inputs '1' and '2'.",
    tool_mode = "sequential"
  ))

  # The calls don't overlap, the first ends before the second starts
  expect_true(res[[1]]$start < res[[1]]$end)
  expect_true(res[[1]]$end < res[[2]]$start)
  expect_true(res[[2]]$start < res[[2]]$end)
})

test_that("$stream(stream='content') yields tool request/result contents", {
  chat <- chat_openai_test()
  tool_current_date <- tool(
    function() "2024-01-01",
    description = "Return the current date"
  )
  chat$register_tool(tool_current_date)

  res <- coro::collect(
    chat$stream(
      "What's the current date in Y-M-D format?",
      stream = "content"
    )
  )

  # 1. Tool request
  # 2. Tool result (paired with request)
  # 3. ...rest of assistant message
  expect_s7_class(res[[1]], ContentToolRequest)
  expect_equal(res[[1]]@tool, tool_current_date)
  expect_s7_class(res[[2]], ContentToolResult)
  expect_equal(res[[2]]@value, "2024-01-01")
  expect_equal(res[[2]]@request, res[[1]])

  for (delta in res[-(1:2)]) {
    expect_s7_class(delta, ContentText)
  }
})

test_that("$stream_async(stream='content', tool_mode='concurrent') yields tool request/result contents concurrently", {
  chat <- chat_openai_test()
  tool_current_date <- tool(
    coro::async(function() {
      coro::await(coro::async_sleep(0.1))
      "2024-01-01"
    }),
    name = "current_date",
    description = "Return the current date"
  )
  chat$register_tool(tool_current_date)

  res <- sync(
    coro::async_collect(
      chat$stream_async(
        "Confirm the current data by calling `current_date` twice.",
        "Write YES if the dates match or NO if not.",
        stream = "content",
        tool_mode = "concurrent"
      )
    )
  )

  # 1. Tool request 1
  expect_s7_class(res[[1]], ContentToolRequest)
  expect_equal(res[[1]]@tool, tool_current_date)
  # 2. Tool request 2
  expect_s7_class(res[[2]], ContentToolRequest)
  expect_equal(res[[2]]@tool, tool_current_date)
  # 3. Tool result 1
  expect_s7_class(res[[3]], ContentToolResult)
  expect_equal(res[[3]]@value, "2024-01-01")
  expect_equal(res[[3]]@request, res[[1]])
  # 4. Tool result 2
  expect_s7_class(res[[4]], ContentToolResult)
  expect_equal(res[[4]]@value, "2024-01-01")
  expect_equal(res[[4]]@request, res[[2]])

  # 5. ...rest of assistant message
  for (delta in res[-(1:4)]) {
    expect_s7_class(delta, ContentText)
  }
})

test_that("$stream_async(stream='content', tool_mode='sequential') yields tool request/result contents sequentially", {
  chat <- chat_openai_test()
  tool_current_date <- tool(
    coro::async(function() {
      coro::await(coro::async_sleep(0.1))
      "2024-01-01"
    }),
    name = "current_date",
    description = "Return the current date"
  )
  chat$register_tool(tool_current_date)

  res <- sync(
    coro::async_collect(
      chat$stream_async(
        "Confirm the current data by calling `current_date` twice.",
        "Write YES if the dates match or NO if not.",
        stream = "content",
        tool_mode = "sequential"
      )
    )
  )

  # 1. Tool request 1
  expect_s7_class(res[[1]], ContentToolRequest)
  expect_equal(res[[1]]@tool, tool_current_date)
  # 2. Tool result 1
  expect_s7_class(res[[2]], ContentToolResult)
  expect_equal(res[[2]]@value, "2024-01-01")
  expect_equal(res[[2]]@request, res[[1]])
  # 3. Tool request 2
  expect_s7_class(res[[3]], ContentToolRequest)
  expect_equal(res[[3]]@tool, tool_current_date)
  # 4. Tool result 2
  expect_s7_class(res[[4]], ContentToolResult)
  expect_equal(res[[4]]@value, "2024-01-01")
  expect_equal(res[[4]]@request, res[[3]])

  # 5. ...rest of assistant message
  for (delta in res[-(1:4)]) {
    expect_s7_class(delta, ContentText)
  }
})

# Invocation ------------------------------------------------------------------

test_that("invoke_tool returns a ContentToolResult", {
  tool_f <- tool(function() 1, name = "my_tool", description = "A tool")

  res <- invoke_tool(
    ContentToolRequest(
      id = "x",
      name = "my_tool",
      arguments = list(),
      tool = tool_f
    )
  )
  expect_s3_class(res, "ellmer::ContentToolResult")
  expect_equal(res@error, NULL)
  expect_false(tool_errored(res))
  expect_equal(res@value, 1)
  expect_s3_class(res@request, "ellmer::ContentToolRequest")
  expect_equal(res@request@id, "x")
  expect_equal(res@request@tool, tool_f)
  expect_equal(res@request@arguments, list())

  res <- invoke_tool(
    ContentToolRequest(
      id = "x",
      name = "my_tool",
      arguments = list(x = 1),
      tool = tool_f
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
  expect_equal(res@request@tool, tool_f)
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
    name = "my_tool",
    description = "A tool that returns ContentToolResult"
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
  tool_f <- tool(function() 1, name = "my_tool", description = "A tool")

  res <- sync(invoke_tool_async(
    ContentToolRequest(
      id = "x",
      name = "my_tool",
      arguments = list(),
      tool = tool_f
    )
  ))
  expect_s3_class(res, "ellmer::ContentToolResult")
  expect_equal(res@error, NULL)
  expect_false(tool_errored(res))
  expect_equal(res@value, 1)
  expect_s3_class(res@request, "ellmer::ContentToolRequest")
  expect_equal(res@request@id, "x")
  expect_equal(res@request@tool, tool_f)
  expect_equal(res@request@arguments, list())

  res <- sync(invoke_tool_async(
    ContentToolRequest(
      id = "x",
      name = "my_tool",
      arguments = list(x = 1),
      tool = tool_f
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
  expect_equal(res@request@tool, tool_f)
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
    name = "my_tool",
    description = "A tool that returns ContentToolResult"
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
    function(x, y) out <<- list(x = x, y = y),
    description = "A tool",
    arguments = list(
      x = type_array(type_number()),
      y = type_array(type_string())
    )
  )

  req <- ContentToolRequest(
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
    function(x, y) out <<- list(x = x, y = y),
    description = "A tool",
    arguments = list(
      x = type_array(type_number()),
      y = type_array(type_string())
    )
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
    description = "A tool",
    arguments = list(
      x = type_array(type_number()),
      y = type_array(type_string()),
      z = type_array(type_string(), required = FALSE)
    )
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
    description = "A tool",
    arguments = list(
      x = type_array(type_number()),
      y = type_array(type_string()),
      z = type_array(type_string(), required = FALSE)
    )
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
