# See test-chat-tools.R for tests of tool calling

test_that("can get and set the system prompt", {
  chat <- chat_openai_test()
  chat$set_turns(list(
    UserTurn("Hi"),
    AssistantTurn("Hello")
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
  chat1 <- chat_openai_test(system_prompt = NULL)
  expect_equal(chat1$last_turn("system"), NULL)

  chat2 <- chat_openai_test(system_prompt = "You are from New Zealand")
  expect_equal(
    chat2$last_turn("system"),
    SystemTurn("You are from New Zealand")
  )
})

test_that("can get and set turns", {
  chat <- chat_openai_test()
  expect_equal(chat$get_turns(), list())

  turns <- list(UserTurn(), AssistantTurn())
  chat$set_turns(turns)
  expect_equal(chat$get_turns(), list(UserTurn(), AssistantTurn()))
})

test_that("can get model", {
  chat <- chat_openai_test(model = "abc")
  expect_equal(chat$get_model(), "abc")
})

test_that("setting turns usually preserves, but can set system prompt", {
  chat <- chat_openai_test(system_prompt = "You're a funny guy")
  chat$set_turns(list())
  expect_equal(chat$get_system_prompt(), "You're a funny guy")

  chat$set_turns(list(SystemTurn(list(ContentText("You're a cool guy")))))
  expect_equal(chat$get_system_prompt(), "You're a cool guy")
})


test_that("can perform a simple batch chat", {
  chat <- chat_openai_test()

  result <- chat$chat("What's 1 + 1. Just give me the answer, no punctuation")
  expect_equal(result, ellmer_output("2"))
  expect_equal(chat$last_turn()@contents[[1]]@text, "2")
})

test_that("can chat with a single prompt", {
  chat <- chat_openai_test()
  expect_no_error(chat$chat(interpolate("What's 1 + 1?")))
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

test_that("can extract structured data", {
  person <- type_object(name = type_string(), age = type_integer())

  chat <- chat_openai_test()
  data <- chat$chat_structured("John, age 15, won first prize", type = person)
  expect_equal(data, list(name = "John", age = 15))
})

test_that("can extract structured data (async)", {
  person <- type_object(name = type_string(), age = type_integer())

  chat <- chat_openai_test()
  data <- sync(chat$chat_structured_async(
    "John, age 15, won first prize",
    type = person
  ))
  expect_equal(data, list(name = "John", age = 15))
})

test_that("chat_structured() doesn't require a prompt", {
  chat <- chat_openai_test()
  chat$chat("What's the biggest city in the world? What country is it in?")

  out <- chat$chat_structured(
    type = type_object(
      city = type_string(),
      county = type_string()
    )
  )
  expect_equal(out, list(city = "Tokyo", county = "Japan"))
})

test_that("has a basic print method", {
  chat <- chat_openai_test()
  chat$set_turns(list(
    UserTurn("What's 1 + 1?\nWhat's 1 + 2?"),
    AssistantTurn("2\n\n3", tokens = c(10, 5, 5))
  ))
  expect_snapshot(chat)
})

test_that("print method shows interrupted for partial turns", {
  chat <- chat_openai_test(model = "gpt-4o", system_prompt = NULL)
  chat$set_turns(list(
    UserTurn("Input 1"),
    AssistantTurn("Output 1", tokens = c(15000, 500, 0), cost = 0.2),
    UserTurn("Input 2"),
    AssistantPartialTurn("Partial output...")
  ))
  expect_snapshot(chat)
})

test_that("print method shows custom reason for partial turns", {
  chat <- chat_openai_test(model = "gpt-4o", system_prompt = NULL)
  chat$set_turns(list(
    UserTurn("Input 1"),
    AssistantPartialTurn("Partial output...", reason = "cancelled")
  ))
  expect_snapshot(chat)
})

test_that("print method shows cumulative tokens & cost", {
  chat <- chat_openai_test(model = "gpt-4o", system_prompt = NULL)
  chat$set_turns(list(
    UserTurn("Input 1"),
    AssistantTurn("Output 1", tokens = c(15000, 500, 0), cost = 0.2),
    UserTurn("Input 2"),
    AssistantTurn("Output 1", tokens = c(30000, 1000, 0), cost = 0.1)
  ))
  expect_snapshot(chat)
})

test_that("can compute costs", {
  chat <- chat_openai_test(model = "gpt-4o", system_prompt = NULL)
  chat$set_turns(list(
    UserTurn("Input 1"),
    AssistantTurn("Output 1", tokens = c(15000, 500, 0), cost = 0.2),
    UserTurn("Input 2"),
    AssistantTurn("Output 1", tokens = c(30000, 1000, 0), cost = 0.1)
  ))

  expect_equal(chat$get_cost(), dollars(0.3))
  expect_equal(chat$get_cost("last"), dollars(0.1))

  details <- chat$get_tokens()
  expect_equal(details$cost, dollars(c(0.2, 0.1)))
  expect_equal(details$input, c(15000, 30000))
  expect_equal(details$output, c(500, 1000))
  expect_equal(details$cached_input, c(0, 0))

  expect_snapshot(details)
})

test_that("can optionally echo", {
  chat <- chat_openai_test("Repeat the input back to me exactly", echo = TRUE)
  expect_output(chat$chat("Echo this."), "Echo this.")
  expect_output(chat$chat("Echo this.", echo = FALSE), NA)

  chat <- chat_openai_test("Repeat the input back to me exactly")
  expect_output(chat$chat("Echo this."), NA)
  expect_output(chat$chat("Echo this.", echo = TRUE), "Echo this.")
})

test_that("can retrieve last_turn for user and assistant", {
  chat <- chat_openai_test()
  expect_equal(chat$last_turn("user"), NULL)
  expect_equal(chat$last_turn("assistant"), NULL)

  chat$chat("Hi")
  expect_equal(chat$last_turn("user")@role, "user")
  expect_equal(chat$last_turn("assistant")@role, "assistant")
})

test_that("api_headers parameter works correctly", {
  chat <- chat_openai_test(api_headers = c("X-Test" = "value"))
  expect_equal(chat$get_provider()@extra_headers, c("X-Test" = "value"))

  req <- chat_request(chat$get_provider())
  expect_equal(req_get_headers(req), list("X-Test" = "value"))
})

test_that("assistant turns track duration", {
  vcr::local_cassette("chat-duration")

  chat <- chat_openai_test()
  chat$chat("What's 1 + 1?")

  assistant_turn <- chat$last_turn()

  # These assistant durations are usually not NA, but are during replay (#479)
  expect_true(is.na(assistant_turn@duration) || assistant_turn@duration > 0)
})

# stream_controller() ----------------------------------------------------------

test_that("stream_controller() creates correct object", {
  ctrl <- stream_controller()
  expect_s3_class(ctrl, "ellmer_stream_controller")
  expect_false(ctrl$cancelled)
  expect_null(ctrl$reason)
  expect_true(is.function(ctrl$cancel))
  expect_true(is.function(ctrl$reset))
})

test_that("stream_controller()$cancel() sets cancelled to TRUE", {
  ctrl <- stream_controller()
  ctrl$cancel()
  expect_true(ctrl$cancelled)
  expect_equal(ctrl$reason, "cancelled")
})

test_that("stream_controller()$cancel() accepts a custom reason", {
  ctrl <- stream_controller()
  ctrl$cancel(reason = "timeout")
  expect_true(ctrl$cancelled)
  expect_equal(ctrl$reason, "timeout")
})

test_that("stream_controller()$reset() clears cancelled state and reason", {
  ctrl <- stream_controller()
  ctrl$cancel(reason = "timeout")
  expect_true(ctrl$cancelled)
  expect_equal(ctrl$reason, "timeout")
  ctrl$reset()
  expect_false(ctrl$cancelled)
  expect_null(ctrl$reason)
})

test_that("as_controller() resets a pre-cancelled controller", {
  ctrl <- stream_controller()
  ctrl$cancel()
  result <- expect_silent(as_controller(ctrl, reset = TRUE))
  expect_false(ctrl$cancelled)
  expect_identical(result, ctrl)
})

test_that("stream() rejects non-controller object", {
  chat <- chat_openai_test()
  expect_snapshot(error = TRUE, {
    coro::collect(chat$stream("hi", controller = TRUE))
  })
})

test_that("stream_async() rejects non-controller object", {
  chat <- chat_openai_test()
  expect_snapshot(error = TRUE, {
    sync(coro::async_collect(chat$stream_async("hi", controller = list())))
  })
})

test_that("as_controller() accepts a valid stream_controller() or NULL", {
  ctrl <- stream_controller()
  expect_identical(as_controller(ctrl), ctrl)

  default <- as_controller(NULL)
  expect_false(default$cancelled)
  expect_null(default$reason)
})

test_that("stream_controller() rejects invalid cancelled values", {
  ctrl <- stream_controller()
  expect_error(ctrl$cancelled <- "banana")
  expect_error(ctrl$cancelled <- NA)
  expect_error(ctrl$cancelled <- c(TRUE, FALSE))
})

test_that("stream_controller() rejects invalid reason values", {
  ctrl <- stream_controller()
  expect_error(ctrl$reason <- 123)
  expect_error(ctrl$reason <- NA_character_)
  expect_error(ctrl$reason <- c("a", "b"))
})

test_that("stream_controller() environment is locked", {
  ctrl <- stream_controller()
  expect_error(ctrl$typo <- TRUE)
})

test_that("finalize_turn() merges adjacent ContentText", {
  chat <- chat_openai_test()
  acc <- TurnAccumulator$new(
    chat,
    chat$.__enclos_env__$private,
    stream_controller()
  )

  user_turn <- Turn("user", list(ContentText("hi")))
  acc$begin_turn(user_turn)
  acc$update_turn(ContentText("Hello "))
  acc$update_turn(ContentText("world"))
  acc$finalize_turn()

  turn <- chat$last_turn()
  expect_s7_class(turn, AssistantPartialTurn)
  expect_length(turn@contents, 1)
  expect_equal(turn@text, "Hello world")
  expect_equal(turn@reason, "interrupted")
  # No token data
  expect_true(all(is.na(turn@tokens)))
  expect_true(is.na(turn@cost))
})

test_that("finalize_turn() uses controller reason", {
  chat <- chat_openai_test()
  ctrl <- stream_controller()
  ctrl$cancel(reason = "timeout")
  acc <- TurnAccumulator$new(chat, chat$.__enclos_env__$private, ctrl)

  user_turn <- Turn("user", list(ContentText("hi")))
  acc$begin_turn(user_turn)
  acc$update_turn(ContentText("partial"))
  acc$finalize_turn()

  turn <- chat$last_turn()
  expect_s7_class(turn, AssistantPartialTurn)
  expect_equal(turn@reason, "timeout")
})

test_that("finalize_turn() is a no-op for complete turns", {
  chat <- chat_openai_test()
  acc <- TurnAccumulator$new(
    chat,
    chat$.__enclos_env__$private,
    stream_controller()
  )

  user_turn <- Turn("user", list(ContentText("hi")))
  chat$add_turn(
    user_turn,
    AssistantTurn(contents = list(ContentText("done"))),
    log_tokens = FALSE
  )
  # Manually set turn_idx so finalize_turn has something to check
  acc$.__enclos_env__$private$turn_idx <- 2L

  acc$finalize_turn()
  turn <- chat$last_turn()

  expect_s7_class(turn, AssistantTurn)
  expect_false(S7_inherits(turn, AssistantPartialTurn))
})

test_that("update_turn() appends content incrementally", {
  chat <- chat_openai_test()
  acc <- TurnAccumulator$new(
    chat,
    chat$.__enclos_env__$private,
    stream_controller()
  )

  user_turn <- Turn("user", list(ContentText("hi")))
  acc$begin_turn(user_turn)
  acc$update_turn(ContentText("a"))
  acc$update_turn(ContentText("b"))

  turn <- chat$last_turn()
  expect_length(turn@contents, 2)
  expect_equal(turn@contents[[1]]@text, "a")
  expect_equal(turn@contents[[2]]@text, "b")
})

test_that("merge_content_text() merges adjacent text, preserves non-text", {
  contents <- list(
    ContentText("a"),
    ContentText("b"),
    ContentThinking("thought"),
    ContentText("c")
  )
  merged <- merge_content_text(contents)

  expect_length(merged, 3)
  expect_s7_class(merged[[1]], ContentText)
  expect_equal(merged[[1]]@text, "ab")
  expect_s7_class(merged[[2]], ContentThinking)
  expect_s7_class(merged[[3]], ContentText)
  expect_equal(merged[[3]]@text, "c")
})
