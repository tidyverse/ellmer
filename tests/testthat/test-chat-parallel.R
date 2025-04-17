test_that("can chat in parallel", {
  chat <- chat_openai_test("Just give me answers, no punctuation")
  chats <- chat_parallel(chat, list("What's 1 + 1?", "What's 2 + 2?"))

  expect_type(chats, "list")
  expect_length(chats, 2)

  expect_s3_class(chats[[1]], "Chat")
  expect_s3_class(chats[[2]], "Chat")

  expect_equal(chats[[1]]$last_turn()@contents[[1]]@text, "2")
  expect_equal(chats[[2]]$last_turn()@contents[[1]]@text, "4")
})

test_that("can call tools in parallel", {
  chat <- chat_openai_test("Just give me answers, no punctuation")
  mysample <- function() 1
  chat$register_tool(tool(mysample, "Picks a random number between 1 and 10"))

  chats <- chat_parallel(chat, rep(list("Pick a number between 1 and 10"), 2))

  turns_1 <- chats[[1]]$get_turns()
  expect_s3_class(turns_1[[2]]@contents[[1]], "ellmer::ContentToolRequest")
  expect_s3_class(turns_1[[3]]@contents[[1]], "ellmer::ContentToolResult")
  expect_equal(contents_text(turns_1[[4]]), "1")
})

test_that("can have uneven number of turns", {
  chat <- chat_openai_test("Just give me answers, no punctuation")
  i <- 0
  mysample <- function() {
    i <<- i + 1
    if (i == 1) {
      1
    } else if (i == 2) {
      10
    } else {
      sample(10, 1)
    }
  }
  chat$register_tool(tool(mysample, "Picks a random number between 1 and 10"))

  prompt <- list(paste0(
    "Pick a random number between 1 and 10.",
    "If the number is less than 5, you're done. If it's more than 5, pick 2 more."
  ))
  prompts <- rep(prompt, 2)

  chats <- chat_parallel(chat, prompts)
  expect_length(chats[[1]]$get_turns(), 4)
  expect_length(chats[[2]]$get_turns(), 6)
})
