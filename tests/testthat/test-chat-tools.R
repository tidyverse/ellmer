# Registry ---------------------------------------------------------------------

test_that("can only register tools", {
  chat <- chat_openai_test()
  tool_def <- tool(function() 1, "tool")

  expect_snapshot(error = TRUE, {
    chat$register_tool(1)
    chat$register_tools(1)
    chat$register_tools(list(tool_def, 1))
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

  # set_tools() throws with helpful message if given just a tool
  expect_snapshot(
    error = TRUE,
    chat$set_tools(tools[[1]])
  )

  # set_tools() throws with helpful message if not all items are tools
  expect_snapshot(
    error = TRUE,
    chat$set_tools(c(tools, list("foo")))
  )
})

# Execution --------------------------------------------------------------------

test_that("can handle parallel tools", {
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

  expect_snapshot(
    . <- chat$chat(
      "What are Joe and Hadley's favorite colors?",
      "Write 'Joe ____ Hadley ____'. Use 'unknown' if you don't know.",
      echo = "output"
    )
  )
})

test_that("tool calls can be rejected via the tool function", {
  chat <- chat_openai_test()

  chat$register_tool(tool(
    function(user) if (user == "Joe") tool_reject() else "red",
    name = "user_favorite_color",
    description = "Find out a user's favorite color",
    arguments = list(user = type_string("User's name"))
  ))

  expect_snapshot(
    . <- chat$chat(
      "What are Joe and Hadley's favorite colors?",
      "Write 'Joe ____ Hadley ____'. Use 'unknown' if you don't know.",
      echo = "output"
    )
  )
})

# Async ------------------------------------------------------------------------

test_that("can use async tools", {
  chat <- chat_openai_test("Be very terse, not even punctuation.")
  chat$register_tool(tool(
    coro::async(function() "2024-01-01"),
    description = "Return the current date"
  ))

  result <- sync(chat$chat_async("What's the current date in Y-M-D format?"))
  expect_match(result, "2024-01-01")

  # Can't use async tools in sync context
  expect_error(chat$chat("Great. Do it again."), class = "tool_async_error")
})

test_that("chat callbacks for tool requests/results", {
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
