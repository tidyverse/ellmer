test_that("can get and set the system prompt", {
  chat <- chat_openai_test()
  chat$set_turns(list(
    Turn("user", "Hi"),
    Turn("assistant", "Hello")
  ))

  # NULL -> NULL
  chat$set_system_prompt(NULL)
  expect_equal(chat$get_system_prompt(), NULL)

  # NULL -> string
  chat$set_system_prompt("x")
  expect_equal(chat$get_system_prompt(), "x")

  # string -> string
  chat$set_system_prompt("y")
  expect_equal(chat$get_system_prompt(), "y")

  # string -> NULL
  chat$set_system_prompt(NULL)
  expect_equal(chat$get_system_prompt(), NULL)
})

test_that("system prompt can be a vector", {
  chat <- chat_openai_test(c("This is", "the system prompt"))
  expect_equal(chat$get_system_prompt(), "This is\n\nthe system prompt")
})

test_that("system prompt must be a character vector", {
  expect_snapshot(error = TRUE, {
    chat_openai_test(1)
  })
})

test_that("can retrieve system prompt with last_turn()", {
  chat1 <- chat_openai_test()
  expect_equal(chat1$last_turn("system"), NULL)

  chat2 <- chat_openai(system_prompt = "You are from New Zealand")
  expect_equal(
    chat2$last_turn("system"),
    Turn(
      "system",
      "You are from New Zealand",
      completed = NULL
    )
  )
})

test_that("can get and set turns", {
  chat <- chat_openai()
  expect_equal(chat$get_turns(), list())

  turns <- list(Turn("user"), Turn("assistant"))
  chat$set_turns(turns)
  expect_equal(chat$get_turns(), list(Turn("user"), Turn("assistant")))
})

test_that("can get model", {
  chat <- chat_openai_test(model = "abc")
  expect_equal(chat$get_model(), "abc")
})

test_that("setting turns usually preserves, but can set system prompt", {
  chat <- chat_openai_test(system_prompt = "You're a funny guy")
  chat$set_turns(list())
  expect_equal(chat$get_system_prompt(), "You're a funny guy")

  chat$set_turns(list(Turn("system", list(ContentText("You're a cool guy")))))
  expect_equal(chat$get_system_prompt(), "You're a cool guy")
})


test_that("can perform a simple batch chat", {
  chat <- chat_openai_test()

  result <- chat$chat("What's 1 + 1. Just give me the answer, no punctuation")
  expect_equal(result, "2")
  expect_equal(chat$last_turn()@contents[[1]]@text, "2")
})

test_that("can't chat with multiple prompts", {
  chat <- chat_openai_test()
  prompt <- interpolate("{{x}}", x = 1:2)
  expect_snapshot(error = TRUE, {
    chat$chat(prompt)
  })
})

test_that("can perform a simple async batch chat", {
  chat <- chat_openai_test()

  result <- chat$chat_async(
    "What's 1 + 1. Just give me the answer, no punctuation"
  )
  expect_s3_class(result, "promise")

  result <- sync(result)
  expect_equal(result, "2")
  expect_equal(chat$last_turn()@contents[[1]]@text, "2")
})

test_that("can perform a simple streaming chat", {
  chat <- chat_openai_test()

  chunks <- coro::collect(chat$stream(
    "
    What are the canonical colors of the ROYGBIV rainbow?
    Put each colour on its own line. Don't use punctuation.
  "
  ))
  expect_gt(length(chunks), 2)

  rainbow_re <- "^red *\norange *\nyellow *\ngreen *\nblue *\nindigo *\nviolet *\n?$"
  expect_match(paste(chunks, collapse = ""), rainbow_re, ignore.case = TRUE)
  expect_match(
    chat$last_turn()@contents[[1]]@text,
    rainbow_re,
    ignore.case = TRUE
  )
})

test_that("can perform a simple async batch chat", {
  chat <- chat_openai_test()

  chunks <- coro::async_collect(chat$stream_async(
    "
    What are the canonical colors of the ROYGBIV rainbow?
    Put each colour on its own line. Don't use punctuation.
  "
  ))
  expect_s3_class(chunks, "promise")

  chunks <- sync(chunks)
  expect_gt(length(chunks), 2)
  rainbow_re <- "^red *\norange *\nyellow *\ngreen *\nblue *\nindigo *\nviolet *\n?$"
  expect_match(paste(chunks, collapse = ""), rainbow_re, ignore.case = TRUE)
  expect_match(
    chat$last_turn()@contents[[1]]@text,
    rainbow_re,
    ignore.case = TRUE
  )
})

test_that("can chat in parallel", {
  chat <- chat_openai_test("Just give me answers, no punctuation")
  results <- chat$chat_parallel(list("What's 1 + 1?", "What's 2 + 2?"))

  expect_type(results, "list")
  expect_length(results, 2)

  expect_s3_class(results[[1]], "Chat")
  expect_s3_class(results[[2]], "Chat")

  expect_equal(results[[1]]$last_turn()@contents[[1]]@text, "2")
  expect_equal(results[[2]]$last_turn()@contents[[1]]@text, "4")
})

test_that("can extract structured data", {
  person <- type_object(name = type_string(), age = type_integer())

  chat <- chat_openai_test()
  data <- chat$extract_data("John, age 15, won first prize", type = person)
  expect_equal(data, list(name = "John", age = 15))
})

test_that("can extract data in parallel", {
  person <- type_object(name = type_string(), age = type_integer())

  chat <- chat_openai_test()
  data <- chat$extract_data_parallel(
    list(
      "John, age 15, won first prize",
      "Jane, age 16, won second prize"
    ),
    type = person
  )
  expect_equal(data, data.frame(name = c("John", "Jane"), age = c(15, 16)))
})

test_that("can extract structured data (async)", {
  person <- type_object(name = type_string(), age = type_integer())

  chat <- chat_openai_test()
  data <- sync(chat$extract_data_async(
    "John, age 15, won first prize",
    type = person
  ))
  expect_equal(data, list(name = "John", age = 15))
})

test_that("can retrieve tokens with or without system prompt", {
  chat <- chat_openai_test("abc")
  expect_equal(nrow(chat$get_tokens(FALSE)), 0)
  expect_equal(nrow(chat$get_tokens(TRUE)), 1)

  chat <- chat_openai()
  expect_equal(nrow(chat$get_tokens(FALSE)), 0)
  expect_equal(nrow(chat$get_tokens(TRUE)), 0)
})

test_that("has a basic print method", {
  chat <- chat_openai(
    "You're a helpful assistant that returns very minimal output"
  )
  chat$set_turns(list(
    Turn("user", "What's 1 + 1?\nWhat's 1 + 2?"),
    Turn("assistant", "2\n\n3", tokens = c(15, 5))
  ))
  expect_snapshot(chat)
})

test_that("print method shows cumulative tokens & cost", {
  chat <- chat_openai()
  chat$set_turns(list(
    Turn("user", "Input 1"),
    Turn("assistant", "Output 1", tokens = c(15000, 500)),
    Turn("user", "Input 2"),
    Turn("assistant", "Output 1", tokens = c(30000, 1000))
  ))
  expect_snapshot(chat)

  expect_equal(chat$get_cost(), dollars(0.1275))
  expect_equal(chat$get_cost("last"), dollars(0.085))
})

test_that("can optionally echo", {
  chat <- chat_openai("Repeat the input back to me exactly", echo = TRUE)
  expect_output(chat$chat("Echo this."), "Echo this.")
  expect_output(chat$chat("Echo this.", echo = FALSE), NA)

  chat <- chat_openai("Repeat the input back to me exactly")
  expect_output(chat$chat("Echo this."), NA)
  expect_output(chat$chat("Echo this.", echo = TRUE), "Echo this.")
})

test_that("can retrieve last_turn for user and assistant", {
  chat <- chat_openai()
  expect_equal(chat$last_turn("user"), NULL)
  expect_equal(chat$last_turn("assistant"), NULL)

  chat$chat("Hi")
  expect_equal(chat$last_turn("user")@role, "user")
  expect_equal(chat$last_turn("assistant")@role, "assistant")
})

test_that("chat messages get timestamped in sequence", {
  chat <- chat_openai()

  before_send <- Sys.time()
  chat$chat("What's 1 + 1?")
  after_receive <- Sys.time()
  turns <- chat$get_turns()

  expect_true(turns[[1]]@completed >= before_send)
  expect_true(turns[[1]]@completed <= turns[[2]]@completed)

  expect_true(turns[[2]]@completed >= turns[[1]]@completed)
  expect_true(turns[[2]]@completed <= after_receive)
})

test_that("parallel chat messages get timestamped correctly", {
  chat <- chat_openai()

  before_send <- Sys.time()
  results <- chat$chat_parallel(list("What's 1 + 1?", "What's 2 + 2?"))
  after_receive <- Sys.time()

  turns1 <- results[[1]]$get_turns()
  turns2 <- results[[2]]$get_turns()

  expect_true(turns1[[1]]@completed >= before_send)
  expect_true(turns1[[1]]@completed <= turns1[[2]]@completed)
  expect_true(turns1[[2]]@completed <= after_receive)

  expect_true(turns2[[1]]@completed >= before_send)
  expect_true(turns2[[1]]@completed <= turns2[[2]]@completed)
  expect_true(turns2[[2]]@completed <= after_receive)
})

test_that("async chat messages get timestamped in sequence", {
  chat <- chat_openai()

  before_send <- Sys.time()
  promise <- chat$chat_async("What's 1 + 1?")
  result <- sync(promise)
  after_receive <- Sys.time()

  turns <- chat$get_turns()

  expect_true(turns[[1]]@completed >= before_send)
  expect_true(turns[[1]]@completed <= turns[[2]]@completed)
  expect_true(turns[[2]]@completed <= after_receive)
})

test_that("chat can get and register a list of tools", {
  chat <- chat_openai(api_key = "not required")
  chat2 <- chat_openai(api_key = "not required")

  tools <- list(
    "sys_time" = tool(
      function() strftime(Sys.time(), "%F %T"),
      .description = "Get the current system time",
      .name = "sys_time"
    ),
    "r_version" = tool(
      function() R.version.string,
      .description = "Get the R version of the current session",
      .name = "r_version"
    )
  )

  for (tool in tools) {
    chat$register_tool(tool)
  }

  chat2$set_tools(tools)

  expect_equal(chat$get_tools(), tools)
  expect_equal(chat2$get_tools(), chat$get_tools())

  # action = "replace" overwrites existing tools
  tool_r_major <- tool(
    function() R.version$major,
    .description = "Get the major version of R",
    .name = "r_version_major"
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

test_that("chat warns on tool failures", {
  chat <- chat_openai_test("Be very terse, not even punctuation.")

  chat$register_tool(tool(
    function(user) stop("User denied tool request"),
    "Find out a user's favorite color",
    user = type_string("User's name"),
    .name = "user_favorite_color"
  ))

  expect_snapshot(
    . <- chat$chat("What are Joe, Hadley, Simon, and Tom's favorite colors?"),
    transform = function(value) gsub(" \\(\\w+_[a-z0-9A-Z]+\\)", " (ID)", value)
  )
})

test_that("chat callbacks for tool requests/results", {
  chat <- chat_openai_test("Be very terse, not even punctuation.")

  test_tool <- tool(
    function(user) c("red", "blue")[nchar(user) %% 2 + 1],
    .description = "Find out a user's favorite color",
    user = type_string("User's name"),
    .name = "user_favorite_color"
  )

  chat$register_tool(test_tool)

  last_request <- NULL
  cb_count_request <- 0
  cb_count_result <- 0

  chat$register_callback("tool_request", function(request) {
    cb_count_request <<- cb_count_request + 1
    cli::cli_inform(
      "[{cb_count_request}] Tool request: {request@arguments$user}"
    )

    expect_s7_class(request, ContentToolRequest)
    expect_equal(request@tool, test_tool)
    last_request <<- request
  })
  chat$register_callback("tool_result", function(result) {
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
    chat$register_callback("chat", function(data) NULL)
  })
})

test_that("tool calls can be rejected via `tool_request` callbacks", {
  chat <- chat_openai_test("Be very terse, not even punctuation.")

  chat$register_tool(tool(
    function(user) "red",
    "Find out a user's favorite color",
    user = type_string("User's name"),
    .name = "user_favorite_color"
  ))

  chat$register_callback("tool_request", function(request) {
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
  chat <- chat_openai_test("Be very terse, not even punctuation.")

  chat$register_tool(tool(
    function(user) if (user == "Joe") tool_reject() else "red",
    "Find out a user's favorite color",
    user = type_string("User's name"),
    .name = "user_favorite_color"
  ))

  expect_snapshot(
    . <- chat$chat(
      "What are Joe and Hadley's favorite colors?",
      "Write 'Joe ____ Hadley ____'. Use 'unknown' if you don't know.",
      echo = "output"
    )
  )
})
