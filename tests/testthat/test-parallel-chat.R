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
  expect_equal(data, tibble::tibble(name = c("John", "Jane"), age = c(15, 16)))
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

test_that("parallel_chat logs tokens", {
  local_tokens()
  vcr::local_cassette("parallel-logging")

  chat <- chat_openai_test()
  prompts <- list("Say 1", "Say 2", "Say 3")
  results <- parallel_chat(chat, prompts)

  input_tokens <- map_dbl(results, \(x) x$get_tokens()$input)
  expect_equal(the$tokens$input, sum(input_tokens))
})

test_that("parallel_chat_structured logs tokens", {
  local_tokens()
  vcr::local_cassette("parallel-structured-logging")

  chat <- chat_openai_test()
  type <- type_object(msg = type_string("A greeting message"))
  prompts <- list("Say hi", "Say bye")
  results <- parallel_chat_structured(
    chat,
    prompts,
    type = type,
    include_tokens = TRUE,
    include_cost = TRUE
  )

  expect_equal(the$tokens$input, sum(results$input_tokens))
  expect_equal(the$tokens$output, sum(results$output_tokens))
})

# error handling ---------------------------------------------------------------

test_that("handles errors and NULLs in parallel functions", {
  chat <- chat_openai(
    credentials = \() "test-key",
    base_url = "http://localhost:1234",
    model = "mock"
  )
  prompts <- list("prompt1", "prompt2", "prompt3")
  responses <- list(
    AssistantTurn("Success", tokens = c(10, 20, 0)),
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
    AssistantTurn(list(ContentJson(list(x = 1))), tokens = c(10, 20, 0)),
    simpleError("Request failed"),
    NULL
  )
  type <- type_object(x = type_number())
  out <- parallel_chat_structured(chat, prompts, type)
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 3)
  expect_named(out, c("x", ".error"))
  expect_equal(out$x, c(1, NA, NA))
})

test_that("errors in conversion become warnings", {
  chat <- chat_openai_test()
  provider <- chat$get_provider()
  type <- type_object(x = type_integer())

  turns <- list(
    AssistantTurn(list(ContentJson(data = list(x = 1)))),
    # no json
    AssistantTurn(list(ContentText("Hello"))),
    # invalid json
    AssistantTurn(list(ContentJson(string = "{")))
  )

  expect_snapshot(out <- multi_convert(provider, turns, type = type))
  expect_equal(out$x, c(1, NA, NA))
})

test_that("extraction failures are recorded in `.error` (#1121)", {
  chat <- chat_openai_test()
  provider <- chat$get_provider()
  type <- type_object(
    x = type_array(type_object(item = type_string()))
  )

  turns <- list(
    AssistantTurn(list(ContentJson(list(x = list(list(item = "a")))))),
    # a refusal in prose, which extracts no data, but converts to the same
    # zero-row tibble as an array that's legitimately empty
    AssistantTurn(list(ContentText("I'm sorry, I can't do that."))),
    simpleError("Request failed"),
    NULL
  )

  expect_warning(
    out <- multi_convert(provider, turns, type = type),
    "Failed to extract data from 1/4 turns"
  )
  expect_equal(map_int(out$x, nrow), c(1L, 0L, 0L, 0L))
  expect_null(out$.error[[1]])
  expect_s3_class(out$.error[[2]], "error")
  expect_equal(conditionMessage(out$.error[[3]]), "Request failed")
  expect_null(out$.error[[4]])
})

test_that("incomplete responses are recorded in `.error` (#1126)", {
  chat <- chat_openai_test()
  provider <- chat$get_provider()
  type <- type_object(x = type_integer())

  # The JSON is complete and valid: only the finish reason says the response
  # was cut short, as when a truncated tool call arrives with an empty input
  turns <- list(
    AssistantTurn(list(ContentJson(list(x = 1))), finish_reason = "success"),
    AssistantTurn(list(ContentJson(list(x = 2))), finish_reason = "max_tokens"),
    AssistantTurn(
      list(ContentJson(list(x = 3))),
      finish_reason = "content_filter"
    ),
    AssistantTurn(
      list(ContentJson(list(x = 4))),
      finish_reason = I("who knows")
    )
  )

  expect_snapshot(out <- multi_convert(provider, turns, type = type))
  expect_equal(out$x, c(1, NA, NA, NA))
  expect_null(out$.error[[1]])
  expect_match(conditionMessage(out$.error[[2]]), "max_tokens")
  expect_match(conditionMessage(out$.error[[3]]), "content moderation")
  expect_match(conditionMessage(out$.error[[4]]), "unexpected finish reason")
})

test_that("finish reasons that aren't failures are left alone", {
  chat <- chat_openai_test()
  provider <- chat$get_provider()
  type <- type_object(x = type_integer())

  turns <- map(
    c("success", "tool_use", "stop_sequence", NA_character_),
    \(reason) {
      AssistantTurn(list(ContentJson(list(x = 1))), finish_reason = reason)
    }
  )

  expect_no_warning(out <- multi_convert(provider, turns, type = type))
  expect_equal(out$x, rep(1, 4))
  expect_named(out, "x")
})

test_that("an incomplete response takes precedence over its parse error", {
  chat <- chat_openai_test()
  provider <- chat$get_provider()
  type <- type_object(x = type_integer())

  # Truncated JSON fails to parse, but "premature EOF" describes the symptom
  # and points at the wrong fix
  turns <- list(
    AssistantTurn(
      list(ContentJson(string = '{"x": ')),
      finish_reason = "max_tokens"
    )
  )

  expect_warning(
    out <- multi_convert(provider, turns, type = type),
    "max_tokens"
  )
  expect_match(conditionMessage(out$.error[[1]]), "max_tokens")
})

test_that("usage is still reported for a row with no usable data", {
  chat <- chat_openai_test()
  provider <- chat$get_provider()
  type <- type_object(x = type_integer())

  # The request was performed and billed, whether or not its data survived
  turns <- list(
    AssistantTurn(
      list(ContentJson(list(x = 1))),
      tokens = c(10, 20, 0),
      cost = 0.1,
      finish_reason = "max_tokens"
    ),
    simpleError("Request failed")
  )

  suppressWarnings(
    out <- multi_convert(
      provider,
      turns,
      type = type,
      include_tokens = TRUE,
      include_cost = TRUE
    )
  )
  expect_equal(out$input_tokens, c(10, 0))
  expect_equal(out$output_tokens, c(20, 0))
  expect_equal(out$cost, c(0.1, 0))
})

test_that("assistant turns track duration in parallel", {
  vcr::local_cassette("parallel-duration")

  chat <- chat_openai_test()
  chats <- parallel_chat(chat, list("What's 1 + 1?", "What's 2 + 2?"))

  assistant_duration_1 <- chats[[1]]$last_turn()@duration
  assistant_duration_2 <- chats[[2]]$last_turn()@duration

  # These assistant durations are usually not NA, but are during replay (#479)
  expect_true(is.na(assistant_duration_1) || assistant_duration_1 > 0)
  expect_true(is.na(assistant_duration_2) || assistant_duration_2 > 0)
})
