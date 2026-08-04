test_that("invalid inputs give useful errors", {
  chat <- chat_openai_test()

  expect_snapshot(error = TRUE, {
    chat$chat(question = "Are unicorns real?")
    chat$chat(TRUE)
  })
})

test_that("can create content from a vector", {
  expect_equal(
    as_content(c("a", "b")),
    ContentText("a\n\nb")
  )
})

test_that("turn contents can be converted to text, markdown and HTML", {
  turn <- UserTurn(
    contents = list(
      ContentText("User input."),
      ContentImageInline("image/png", "abcd123"),
      ContentImageRemote("https://example.com/image.jpg", detail = ""),
      ContentJson(list(a = 1:2, b = "apple"))
    )
  )

  expect_snapshot(cat(contents_text(turn)))
  expect_snapshot(cat(contents_markdown(turn)))

  turns <- list(
    turn,
    AssistantTurn(list(ContentText("Here's your answer.")))
  )
  chat <- Chat$new(test_provider())
  chat$set_turns(turns)
  expect_snapshot(cat(contents_markdown(chat)))

  skip_if_not_installed("commonmark")
  expect_snapshot(cat(contents_html(turn)))
})


# Content types ----------------------------------------------------------------

test_that("thinking has useful representations", {
  ct <- ContentThinking("A **thought**.")
  expect_equal(contents_text(ct), NULL)
  expect_equal(format(ct), "<thinking>\nA **thought**.\n</thinking>\n")
  expect_equal(
    contents_markdown(ct),
    "<thinking>\nA **thought**.\n</thinking>\n"
  )
  expect_snapshot(cat(contents_html(ct)))
})

test_that("ContentToolRequest shows converted arguments", {
  my_tool <- tool(
    function(x, y, z) {},
    name = "my_tool",
    description = "A tool",
    arguments = list(
      x = type_array(type_number()),
      y = type_array(type_string()),
      z = type_enum(c("a", "b", "c"))
    )
  )
  content <- ContentToolRequest(
    "id",
    name = "my_tool",
    arguments = list(x = c(1, 2), y = c("a", "b"), z = "a"),
    tool = my_tool
  )
  expect_snapshot(cat(format(content)))

  # and very long arguments are truncated
  content <- ContentToolRequest(
    "id",
    name = "my_tool",
    arguments = list(x = rep(123, 1000), y = c("a", "b"), z = "a"),
    tool = my_tool
  )
  expect_snapshot(cat(format(content)))
})

test_that("ContentToolResult@error requires a string or an error condition", {
  expect_snapshot(error = TRUE, {
    ContentToolResult("id", error = TRUE)
    ContentToolResult("id", error = c("one", "two"))
  })
})

test_that("format(ContentToolResult) truncates with max_lines", {
  request <- ContentToolRequest(id = "id1", name = "my_tool")
  short_value <- "line1\nline2\nline3"
  long_value <- paste0("line", 1:10, collapse = "\n")

  short_result <- ContentToolResult(
    value = short_value,
    request = request
  )
  long_result <- ContentToolResult(
    value = long_value,
    request = request
  )

  # No truncation by default
  expect_match(format(long_result), "line10")

  # tool_max_lines = 5 truncates long output
  formatted <- format(long_result, tool_max_lines = 5)
  expect_match(formatted, "line5")
  expect_no_match(formatted, "line6")

  # Short output unaffected by max_lines
  expect_equal(
    format(short_result, tool_max_lines = 5),
    format(short_result)
  )
})

test_that("format(ContentToolResult, style = 'reprex') uses #> prefix", {
  request <- ContentToolRequest(id = "id1", name = "my_tool")
  result <- ContentToolResult(value = "hello", request = request)

  # Single-line: header + reprex value
  formatted <- format(result, tool_style = "reprex")
  expect_match(cli::ansi_strip(formatted), "#> hello")
  expect_match(cli::ansi_strip(formatted), "tool result")

  # Multi-line: header line then #> prefixed lines
  multi <- ContentToolResult(
    value = "line1\nline2\nline3",
    request = request
  )
  formatted <- format(multi, tool_style = "reprex")
  lines <- strsplit(formatted, "\n")[[1]]
  reprex_lines <- lines[grepl("^#> ", cli::ansi_strip(lines))]
  expect_length(reprex_lines, 3)

  # show = "value" returns just the reprex lines without header
  formatted <- format(multi, show = "value", tool_style = "reprex")
  lines <- strsplit(formatted, "\n")[[1]]
  expect_length(lines, 3)
  expect_true(all(grepl("^#> ", cli::ansi_strip(lines))))

  # reprex + max_lines truncates
  long <- ContentToolResult(
    value = paste0("line", 1:10, collapse = "\n"),
    request = request
  )
  formatted <- format(
    long,
    show = "value",
    tool_style = "reprex",
    tool_max_lines = 3
  )
  lines <- strsplit(formatted, "\n")[[1]]
  expect_length(lines, 4)
  expect_match(cli::ansi_strip(lines[4]), "7 more lines")
})

test_that("truncate_id() shortens long IDs", {
  expect_equal(truncate_id("short"), "short")
  expect_equal(truncate_id("exactly12chr"), "exactly12chr")
  # 8 prefix + ellipsis + 4 suffix
  expect_equal(
    truncate_id("call_abc123xyz99"),
    paste0("call_abc", cli::symbol$ellipsis, "yz99")
  )
  expect_match(truncate_id("a_very_long_tool_id"), "^a_very_l.*l_id$")
})

test_that("truncate_lines() truncates multi-line strings", {
  value <- paste0("line", 1:10, collapse = "\n")

  # NULL max_lines returns unchanged

  expect_equal(truncate_lines(value, NULL), value)

  # Truncates to max_lines + remaining count
  result <- truncate_lines(value, 3)
  lines <- strsplit(result, "\n")[[1]]
  expect_length(lines, 4)
  expect_equal(lines[1:3], paste0("line", 1:3))
  expect_match(lines[4], "7 more lines")

  # Singular "line" when only 1 remains
  result2 <- truncate_lines(paste0("line", 1:4, collapse = "\n"), 3)
  expect_match(strsplit(result2, "\n")[[1]][4], "1 more line")

  # Single-line strings are never truncated
  expect_equal(truncate_lines("one line", 3), "one line")

  # No truncation when under the limit
  short <- "a\nb\nc"
  expect_equal(truncate_lines(short, 5), short)
})
