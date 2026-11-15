# Getting started --------------------------------------------------------

test_that("can make simple request", {
  chat <- chat_deepseek("Be as terse as possible; no punctuation")
  resp <- chat$chat("What is 1 + 1?", echo = FALSE)
  expect_match(resp, "2")
  expect_equal(unname(chat$last_turn()@tokens[1:2] > 0), c(TRUE, TRUE))
})

test_that("can make simple streaming request", {
  chat <- chat_deepseek("Be as terse as possible; no punctuation")
  resp <- coro::collect(chat$stream("What is 1 + 1?"))
  expect_match(paste0(unlist(resp), collapse = ""), "2")
})

test_that("can list models", {
  test_models(models_deepseek)
})

# Common provider interface -----------------------------------------------

test_that("defaults are reported", {
  expect_snapshot(. <- chat_deepseek())
})

test_that("supports tool calling", {
  chat_fun <- chat_deepseek

  test_tools_simple(chat_fun)
})

test_that("reasoning effort reaches the request body", {
  chat <- chat_deepseek(
    model = "deepseek-flash",
    credentials = function() "test-key",
    params = params(reasoning_effort = "max")
  )
  req <- chat_request(chat$get_provider(), chat$get_model_object())
  expect_equal(req_get_body(req)$reasoning_effort, "max")
})

test_that("assistant history preserves thinking alongside tools and answers", {
  provider <- chat_deepseek(model = "deepseek-flash")$get_provider()
  tool_turn <- AssistantTurn(list(
    ContentThinking("Use the calculator."),
    ContentToolRequest(id = "call_1", name = "calculator")
  ))
  message <- as_json(provider, tool_turn)[[1]]
  expect_equal(message$reasoning_content, "Use the calculator.")
  expect_equal(message$tool_calls[[1]]$id, "call_1")

  turn <- AssistantTurn(list(
    ContentThinking("First. "),
    ContentThinking("Second."),
    ContentText("Answer")
  ))
  expect_equal(
    as_json(provider, turn),
    list(list(
      role = "assistant",
      content = "Answer",
      reasoning_content = "First. Second."
    ))
  )
  expect_equal(
    as_json(provider, AssistantTurn("Answer")),
    list(list(
      role = "assistant",
      content = "Answer"
    ))
  )

  provider@preserve_thinking <- FALSE
  expect_equal(
    as_json(provider, turn),
    list(list(role = "assistant", content = "Answer"))
  )
})

# # Doesn't support data extraction
# test_that("can extract data", {
#   chat_fun <- chat_deepseek

#   test_data_extraction(chat_fun)
# })

# # Doesn't support images
# test_that("can use images", {
#   chat_fun <- chat_deepseek_test

#   test_images_inline(chat_fun)
#   test_images_remote(chat_fun)
# })
