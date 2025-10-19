test_that("can chat in parallel", {
  vcr::local_cassette("parallel-basic")

  chat <- chat_openai_test()
  chats <- parallel_chat(chat, list("What's 1 + 1?", "What's 2 + 2?"))

  expect_type(chats, "list")
  expect_length(chats, 2)

  expect_s3_class(chats[[1]], "Chat")
  expect_s3_class(chats[[2]], "Chat")

  expect_equal(chats[[1]]$last_turn()@contents[[1]]@text, "2")
  expect_equal(chats[[2]]$last_turn()@contents[[1]]@text, "4")
})

test_that("can just get text parallel ", {
  vcr::local_cassette("parallel-basic")

  chat <- chat_openai_test()
  out <- parallel_chat_text(chat, list("What's 1 + 1?", "What's 2 + 2?"))
  expect_equal(out, c("2", "4"))
})

test_that("can call tools in parallel", {
  vcr::local_cassette("parallel-tool")

  prompts <- rep(list("Roll the dice, please! Reply with 'You rolled ____'"), 2)

  chat <- chat_openai_test()
  chat$register_tool(tool(
    counter(),
    name = "roll",
    description = "Rolls a six-sided die."
  ))
  chats <- parallel_chat(chat, prompts)

  turns_1 <- chats[[1]]$get_turns()
  expect_s3_class(turns_1[[2]]@contents[[1]], "ellmer::ContentToolRequest")
  expect_s3_class(turns_1[[3]]@contents[[1]], "ellmer::ContentToolResult")
  expect_equal(contents_text(turns_1[[4]]), "You rolled 1")

  turns_1 <- chats[[2]]$get_turns()
  expect_equal(contents_text(turns_1[[4]]), "You rolled 2")
})

test_that("can have uneven number of turns", {
  vcr::local_cassette("parallel-tool-uneven")

  prompts <- list(
    "Roll the dice, please! Reply with 'You rolled ____'",
    "reply with the word 'boop'",
    "Roll the dice, please! Reply with 'You rolled ____'",
    "reply with the word 'beep'"
  )

  chat <- chat_openai_test()
  chat$register_tool(tool(
    counter(),
    name = "roll",
    description = "Rolls a six-sided die."
  ))
  chats <- parallel_chat(chat, prompts)

  lengths <- map_int(chats, \(chat) length(chat$get_turns()))
  expect_equal(lengths, c(4, 2, 4, 2))

  text <- map_chr(chats, \(chat) chat$last_turn()@text)
  expect_equal(text, c("You rolled 1", "boop", "You rolled 2", "beep"))
})

# structured data --------------------------------------------------------------

test_that("can extract data in parallel", {
  vcr::local_cassette("parallel-data")

  person <- type_object(name = type_string(), age = type_integer())

  chat <- chat_openai_test()
  data <- parallel_chat_structured(
    chat,
    list("John, age 15", "Jane, age 16"),
    type = person
  )
  expect_equal(data, data.frame(name = c("John", "Jane"), age = c(15, 16)))
})

test_that("can get tokens", {
  vcr::local_cassette("parallel-data")
  person <- type_object(name = type_string(), age = type_integer())
  chat <- chat_openai_test()

  data <- parallel_chat_structured(
    chat,
    list("John, age 15", "Jane, age 16"),
    type = person,
    include_tokens = TRUE
  )
  # These are pretty weak, but it's hard to know how to do better.
  expect_contains(names(data), c("input_tokens", "output_tokens"))
  expect_equal(data$input_tokens > 0, c(TRUE, TRUE))
  expect_equal(data$output_tokens > 0, c(TRUE, TRUE))
})

test_that("can get cost", {
  vcr::local_cassette("parallel-data")
  person <- type_object(name = type_string(), age = type_integer())
  chat <- chat_openai_test()

  data <- parallel_chat_structured(
    chat,
    list("John, age 15", "Jane, age 16"),
    type = person,
    include_cost = TRUE
  )
  expect_contains(names(data), "cost")
  expect_equal(data$cost > 0, c(TRUE, TRUE))
})

test_that("can get tokens & cost", {
  vcr::local_cassette("parallel-data")
  person <- type_object(name = type_string(), age = type_integer())
  chat <- chat_openai_test()

  data <- parallel_chat_structured(
    chat,
    list("John, age 15", "Jane, age 16"),
    type = person,
    include_cost = TRUE,
    include_tokens = TRUE
  )
  expect_contains(names(data), c("input_tokens", "output_tokens", "cost"))
})

# error handling ---------------------------------------------------------------

test_that("handles errors and NULLs in parallel functions", {
  chat <- chat_openai(
    api_key = "test-key",
    base_url = "http://localhost:1234",
    model = "mock"
  )
  prompts <- list("prompt1", "prompt2", "prompt3")
  responses <- list(
    Turn("assistant", "Success"),
    simpleError("Request failed"),
    NULL
  )
  local_mocked_bindings(parallel_turns = function(...) responses)

  chats <- parallel_chat(chat, prompts)
  expect_length(chats, 3)
  expect_s3_class(chats[[1]], "Chat")
  expect_s3_class(chats[[2]], "error")
  expect_null(chats[[3]])

  expect_equal(parallel_chat_text(chat, prompts), c("Success", NA, NA))

  responses <- list(
    Turn("assistant", list(ContentJson(list(x = 1))), tokens = c(10, 20, 0)),
    simpleError("Request failed"),
    NULL
  )
  type <- type_object(x = type_number())
  out <- parallel_chat_structured(chat, prompts, type, on_error = "continue")
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 3)
  expect_named(out, c("x", ".error"))
  expect_equal(out$x, c(1, NA, NA))
})

test_that("on_error = 'return' returns immediately on first error", {
  chat <- chat_openai(
    api_key = "test-key",
    base_url = "http://localhost:1234",
    model = "mock"
  )
  prompts <- list("prompt1", "prompt2", "prompt3")

  # Mock responses: 2 successes, 1 error
  responses_return <- list(
    Turn("assistant", "Success 1"),
    Turn("assistant", "Success 2"),
    simpleError("Request failed")
  )

  responses_continue <- list(
    Turn("assistant", "Success 1"),
    Turn("assistant", "Success 2"),
    simpleError("Request failed")
  )

  # Test on_error = "return": should return all responses including errors
  local_mocked_bindings(parallel_turns = function(...) responses_return)
  chats_return <- parallel_chat(chat, prompts, on_error = "return")

  expect_length(chats_return, 3)
  expect_s3_class(chats_return[[1]], "Chat")
  expect_s3_class(chats_return[[2]], "Chat")
  expect_s3_class(chats_return[[3]], "error")
  expect_equal(chats_return[[1]]$last_turn()@text, "Success 1")
  expect_equal(chats_return[[2]]$last_turn()@text, "Success 2")

  # Test on_error = "continue": should also return all responses including errors
  local_mocked_bindings(parallel_turns = function(...) responses_continue)
  chats_continue <- parallel_chat(chat, prompts, on_error = "continue")

  expect_length(chats_continue, 3)
  expect_s3_class(chats_continue[[1]], "Chat")
  expect_s3_class(chats_continue[[2]], "Chat")
  expect_s3_class(chats_continue[[3]], "error")
  expect_equal(chats_continue[[1]]$last_turn()@text, "Success 1")
  expect_equal(chats_continue[[2]]$last_turn()@text, "Success 2")
})

test_that("on_error = 'return' in parallel_chat_structured stops at first error", {
  chat <- chat_openai(
    api_key = "test-key",
    base_url = "http://localhost:1234",
    model = "mock"
  )
  prompts <- list("prompt1", "prompt2", "prompt3", "prompt4", "prompt5")
  person <- type_object(name = type_string(), age = type_integer())

  # Mock responses for on_error = "return": 2 successes, 1 error, then 2 more successes
  # The last 2 successes should be filtered out when on_error = "return"
  responses_return <- list(
    Turn("assistant", list(ContentJson(list(name = "Alice", age = 30))), tokens = c(10, 20, 0)),
    Turn("assistant", list(ContentJson(list(name = "Bob", age = 25))), tokens = c(10, 20, 0)),
    simpleError("Request failed"),
    Turn("assistant", list(ContentJson(list(name = "Charlie", age = 35))), tokens = c(10, 20, 0)),
    Turn("assistant", list(ContentJson(list(name = "Diana", age = 40))), tokens = c(10, 20, 0))
  )

  local_mocked_bindings(parallel_turns = function(...) responses_return)
  out_return <- parallel_chat_structured(chat, prompts, person, on_error = "return")

  # Should only have 3 rows (up to and including the error at position 3)
  expect_equal(nrow(out_return), 3)
  expect_equal(out_return$name, c("Alice", "Bob", NA))
  expect_equal(out_return$age, c(30, 25, NA))
  expect_contains(names(out_return), ".error")

  # Mock responses for on_error = "continue": all 5 requests executed
  responses_continue <- list(
    Turn("assistant", list(ContentJson(list(name = "Alice", age = 30))), tokens = c(10, 20, 0)),
    Turn("assistant", list(ContentJson(list(name = "Bob", age = 25))), tokens = c(10, 20, 0)),
    simpleError("Request failed"),
    Turn("assistant", list(ContentJson(list(name = "Charlie", age = 35))), tokens = c(10, 20, 0)),
    Turn("assistant", list(ContentJson(list(name = "Diana", age = 40))), tokens = c(10, 20, 0))
  )

  local_mocked_bindings(parallel_turns = function(...) responses_continue)
  out_continue <- parallel_chat_structured(chat, prompts, person, on_error = "continue")

  # Should have all 5 rows (including results after the error)
  expect_equal(nrow(out_continue), 5)
  expect_equal(out_continue$name, c("Alice", "Bob", NA, "Charlie", "Diana"))
  expect_equal(out_continue$age, c(30, 25, NA, 35, 40))
})
