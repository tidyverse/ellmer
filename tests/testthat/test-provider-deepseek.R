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

test_that("code execution content serializes as ordinary tool calls", {
  stub <- ProviderDeepSeek(name = "", base_url = "", model = "")

  req <- ContentToolRequestCode(
    id = "srvtoolu_1",
    name = "bash_code_execution",
    arguments = list(command = "ls"),
    json = list(type = "server_tool_use", id = "srvtoolu_1")
  )
  res <- ContentToolResponseCode(
    value = "file.txt",
    request = req,
    json = list(
      type = "bash_code_execution_tool_result",
      tool_use_id = "srvtoolu_1"
    )
  )

  # A paused programmatic turn: server-side request, no text
  result <- as_json(stub, AssistantTurn(list(req)))
  expect_equal(result[[1]]$role, "assistant")
  expect_equal(
    result[[1]]$tool_calls[[1]]$`function`$name,
    "bash_code_execution"
  )

  # Request and result in the same turn
  result <- as_json(stub, AssistantTurn(list(req, res)))
  expect_equal(
    result[[2]],
    list(role = "tool", content = "file.txt", tool_call_id = "srvtoolu_1")
  )
})
