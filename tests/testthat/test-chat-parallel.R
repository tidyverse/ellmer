test_that("can chat in parallel", {
  chat <- chat_openai_test("Be terse.", model = "gpt-4.1-nano")
  chats <- chat_parallel(chat, list("What's 1 + 1?", "What's 2 + 2?"))

  expect_type(chats, "list")
  expect_length(chats, 2)

  expect_s3_class(chats[[1]], "Chat")
  expect_s3_class(chats[[2]], "Chat")

  expect_equal(chats[[1]]$last_turn()@contents[[1]]@text, "2")
  expect_equal(chats[[2]]$last_turn()@contents[[1]]@text, "4")
})

test_that("can call tools in parallel", {
  prompts <- rep(list("Roll a die."), 2)

  chat <- chat_openai_test("Be terse", model = "gpt-4.1-nano")
  chat$register_tool(tool(counter(), "Rolls a six-sided die.", .name = "roll"))
  chats <- chat_parallel(chat, prompts)

  turns_1 <- chats[[1]]$get_turns()
  expect_s3_class(turns_1[[2]]@contents[[1]], "ellmer::ContentToolRequest")
  expect_s3_class(turns_1[[3]]@contents[[1]], "ellmer::ContentToolResult")
  expect_equal(contents_text(turns_1[[4]]), "You rolled a 1.")

  turns_1 <- chats[[2]]$get_turns()
  expect_equal(contents_text(turns_1[[4]]), "You rolled a 2.")
})

test_that("can have uneven number of turns", {
  prompts <- list(
    "Roll the dice, please! Reply with 'You rolled ____'",
    "reply with the word 'boop'",
    "Roll the dice, please! Reply with 'You rolled ____'",
    "reply with the word 'beep'"
  )

  chat <- chat_openai_test("Be terse.", model = "gpt-4.1-nano")
  chat$register_tool(tool(counter(), "Rolls a six-sided die.", .name = "roll"))
  chats <- chat_parallel(chat, prompts)

  lengths <- map_int(chats, \(chat) length(chat$get_turns()))
  expect_equal(lengths, c(4, 2, 4, 2))

  text <- map_chr(chats, \(chat) chat$last_turn()@text)
  expect_equal(text, c("You rolled 1", "boop", "You rolled 2", "beep"))
})
