fixture_tool_request_turn <- function() {
  req <- ContentToolRequest(id = "x1", name = "my_tool", arguments = list())
  AssistantTurn(list(req))
}

fixture_tool_result_turn <- function() {
  req <- ContentToolRequest(id = "x1", name = "my_tool", arguments = list())
  UserTurn(list(ContentToolResult(value = 1, request = req)))
}

test_that("get_rounds() groups a simple round", {
  turns <- list(UserTurn("Hi"), AssistantTurn("Hello"))
  rounds <- get_rounds(turns)

  expect_length(rounds, 1)
  expect_s7_class(rounds[[1]], Round)
  expect_equal(rounds[[1]]@input, UserTurn("Hi"))
  expect_equal(rounds[[1]]@response, list(AssistantTurn("Hello")))
  expect_equal(rounds[[1]]@complete, TRUE)
})

test_that("get_rounds() groups a tool-calling loop into one round", {
  turns <- list(
    UserTurn("Hi"),
    fixture_tool_request_turn(),
    fixture_tool_result_turn(),
    AssistantTurn("Done")
  )
  rounds <- get_rounds(turns)

  expect_length(rounds, 1)
  expect_equal(rounds[[1]]@input, UserTurn("Hi"))
  expect_length(rounds[[1]]@response, 3)
  expect_equal(rounds[[1]]@complete, TRUE)
})

test_that("a round ending on a tool-result turn is incomplete", {
  turns <- list(
    UserTurn("Hi"),
    fixture_tool_request_turn(),
    fixture_tool_result_turn()
  )
  rounds <- get_rounds(turns)

  expect_length(rounds, 1)
  expect_equal(rounds[[1]]@complete, FALSE)
})

test_that("get_rounds() groups multiple rounds correctly", {
  turns <- list(
    UserTurn("Hi"),
    AssistantTurn("Hello"),
    UserTurn("Bye"),
    AssistantTurn("Goodbye")
  )
  rounds <- get_rounds(turns)

  expect_length(rounds, 2)
  expect_equal(rounds[[1]]@input, UserTurn("Hi"))
  expect_equal(rounds[[2]]@input, UserTurn("Bye"))
  expect_equal(rounds[[1]]@complete, TRUE)
  expect_equal(rounds[[2]]@complete, TRUE)
})

test_that("get_rounds(list()) returns list()", {
  expect_equal(get_rounds(list()), list())
})

test_that("a lone system turn is passed through with no Round", {
  turns <- list(SystemTurn("Be nice"))
  rounds <- get_rounds(turns)

  expect_equal(rounds, list(SystemTurn("Be nice")))
})

test_that("a system turn followed by rounds is grouped correctly", {
  turns <- list(
    SystemTurn("Be nice"),
    UserTurn("Hi"),
    AssistantTurn("Hello")
  )
  rounds <- get_rounds(turns)

  expect_length(rounds, 2)
  expect_equal(rounds[[1]], SystemTurn("Be nice"))
  expect_s7_class(rounds[[2]], Round)
  expect_equal(rounds[[2]]@input, UserTurn("Hi"))
})

test_that("get_rounds() aborts on a leading tool-result turn", {
  turns <- list(fixture_tool_result_turn())
  expect_snapshot(error = TRUE, get_rounds(turns))
})

test_that("Round validator rejects a tool-result turn as input", {
  expect_snapshot(
    error = TRUE,
    Round(input = fixture_tool_result_turn(), response = list())
  )
})
