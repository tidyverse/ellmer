test_that("can get and set the system prompt", {
  chat <- chat_openai(turns = list(
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

test_that("can retrieve system prompt with last_turn()", {
  chat1 <- chat_openai()
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
  chat <- chat_openai(model = "abc")
  expect_equal(chat$get_model(), "abc")
})

test_that("setting turns usually preserves, but can set system prompt", {
  chat <- chat_openai(system_prompt = "You're a funny guy")
  chat$set_turns(list())
  expect_equal(chat$get_system_prompt(), "You're a funny guy")

  chat$set_turns(list(Turn("system", list(ContentText("You're a cool guy")))))
  expect_equal(chat$get_system_prompt(), "You're a cool guy")
})

test_that("can perform a simple batch chat", {
  chat <- chat_openai()

  result <- chat$chat("What's 1 + 1. Just give me the answer, no punctuation")
  expect_equal(result, "2")
  expect_equal(chat$last_turn()@contents[[1]]@text, "2")
})

test_that("can perform a simple async batch chat", {
  chat <- chat_openai()

  result <- chat$chat_async("What's 1 + 1. Just give me the answer, no punctuation")
  expect_s3_class(result, "promise")

  result <- sync(result)
  expect_equal(result, "2")
  expect_equal(chat$last_turn()@contents[[1]]@text, "2")
})

test_that("can perform a simple streaming chat", {
  chat <- chat_openai()

  chunks <- coro::collect(chat$stream("
    What are the canonical colors of the ROYGBIV rainbow?
    Put each colour on its own line. Don't use punctuation.
  "))
  expect_gt(length(chunks), 2)

  rainbow_re <- "^red *\norange *\nyellow *\ngreen *\nblue *\nindigo *\nviolet *\n?$"
  expect_match(paste(chunks, collapse = ""), rainbow_re, ignore.case = TRUE)
  expect_match(chat$last_turn()@contents[[1]]@text, rainbow_re, ignore.case = TRUE)
})

test_that("can perform a simple async batch chat", {
  chat <- chat_openai()

  chunks <- coro::async_collect(chat$stream_async("
    What are the canonical colors of the ROYGBIV rainbow?
    Put each colour on its own line. Don't use punctuation.
  "))
  expect_s3_class(chunks, "promise")

  chunks <- sync(chunks)
  expect_gt(length(chunks), 2)
  rainbow_re <- "^red *\norange *\nyellow *\ngreen *\nblue *\nindigo *\nviolet *\n?$"
  expect_match(paste(chunks, collapse = ""), rainbow_re, ignore.case = TRUE)
  expect_match(chat$last_turn()@contents[[1]]@text, rainbow_re, ignore.case = TRUE)
})

test_that("can chat in parallel", {
  chat <- chat_openai("Just give me answers, no punctuation")
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

  chat <- chat_openai()
  data <- chat$extract_data("John, age 15, won first prize", type = person)
  expect_equal(data, list(name = "John", age = 15))
})

test_that("can extract data in parallel", {
  person <- type_object(name = type_string(), age = type_integer())

  chat <- chat_openai()
  data <- chat$extract_data_parallel(
    list(
      "John, age 15, won first prize",
      "Jane, age 16, won second prize"
    ),
    type = person
  )
  expect_equal(
    data,
    list(
      list(name = "John", age = 15),
      list(name = "Jane", age = 16)
    )
  )
})

test_that("can extract structured data (async)", {
  person <- type_object(name = type_string(), age = type_integer())

  chat <- chat_openai()
  data <- sync(chat$extract_data_async("John, age 15, won first prize", type = person))
  expect_equal(data, list(name = "John", age = 15))
})

test_that("can retrieve tokens with or without system prompt", {
  chat <- chat_openai("abc")
  expect_equal(nrow(chat$tokens(FALSE)), 0)
  expect_equal(nrow(chat$tokens(TRUE)), 1)

  chat <- chat_openai()
  expect_equal(nrow(chat$tokens(FALSE)), 0)
  expect_equal(nrow(chat$tokens(TRUE)), 0)
})

test_that("has a basic print method", {
  chat <- chat_openai(
    "You're a helpful assistant that returns very minimal output",
    turns = list(
      Turn("user", "What's 1 + 1?\nWhat's 1 + 2?"),
      Turn("assistant", "2\n\n3", tokens = c(15, 5))
    )
  )
  expect_snapshot(chat)
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
