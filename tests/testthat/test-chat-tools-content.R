test_that("can expand all contents in a turn", {
  req <- ContentToolRequest(id = "123", name = "my_tool")
  image <- ContentImageInline("image/png", "abc")

  turn <- UserTurn(list(
    ContentText("abc"),
    ContentToolResult(value = image, request = req),
    ContentToolResult(value = "abc", request = req)
  ))
  expect_length(turn_contents_expand(turn)@contents, 1 + 4 + 1)

  turn <- UserTurn(list(
    ContentText("abc"),
    ContentToolResult(value = image, request = req),
    ContentText("ghi"),
    ContentToolResult(value = image, request = req)
  ))
  expect_length(turn_contents_expand(turn)@contents, 1 + 4 + 1 + 4)

  turn <- UserTurn(list(
    ContentText("abc"),
    ContentToolResult(value = list(image, image), request = req),
    ContentText("ghi")
  ))
  expect_length(
    turn_contents_expand(turn)@contents,
    1 + (1 + 1 + 3 + 3 + 1) + 1
  )
})

test_that("expanding generates useful JSON", {
  req <- ContentToolRequest(id = "123", name = "my_tool")
  image <- ContentImageInline("image/png", "abc")
  provider <- ProviderOpenAI("name", "model", "base_url")

  expanded_simple <- expand_tool_value(req, image)
  expect_snapshot(print_json(as_json(provider, UserTurn(expanded_simple))))

  expanded_list <- expand_tool_values(req, list(image, image))
  expect_snapshot(print_json(as_json(provider, UserTurn(expanded_list))))
})

test_that("can expand tool with single value", {
  req <- ContentToolRequest(id = "123", name = "my_tool")
  image <- ContentImageInline("image/png", "abc")

  expanded <- expand_tool_value(req, image)
  expect_s7_class(expanded[[1]], ContentToolResult)
  expect_equal(expanded[[1]]@request, req)
  expect_s7_class(expanded[[2]], ContentText) # <tool-content>
  expect_equal(expanded[[3]], image)
  expect_s7_class(expanded[[4]], ContentText) # </tool-content>
})

test_that("can expand tool with multiple values", {
  req <- ContentToolRequest(id = "123", name = "my_tool")
  image1 <- ContentImageInline("image/png", "abc")
  image2 <- ContentImageInline("image/png", "def")

  expanded <- expand_tool_values(req, list(image1, image2))
  expect_s7_class(expanded[[1]], ContentToolResult)
  expect_equal(expanded[[1]]@request, req)
  expect_s7_class(expanded[[2]], ContentText) # <tool-contents>
  expect_s7_class(expanded[[3]], ContentText) # <tool-content>
  expect_equal(expanded[[4]], image1)
  expect_s7_class(expanded[[5]], ContentText) # </tool-content>
  expect_s7_class(expanded[[6]], ContentText) # <tool-content>
  expect_equal(expanded[[7]], image2)
  expect_s7_class(expanded[[8]], ContentText) # </tool-content>
  expect_s7_class(expanded[[9]], ContentText) # </tool-contents>
})
