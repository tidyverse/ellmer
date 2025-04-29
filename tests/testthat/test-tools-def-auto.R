test_that("help topic extraction works", {
  print_help <- get_help_text("print")
  expect_type(print_help, "character")
  expect_gt(nchar(print_help), 50)

  expect_identical(get_help_text("print", "base"), print_help)
})

test_that("roxygen2 comment extraction works", {
  sys.source(test_path("tools-def.R"), environment(), keep.source = TRUE)
  aliased_function <- has_roxygen_comments

  expect_snapshot(extract_comments_and_signature(has_roxygen_comments))
  expect_snapshot(extract_comments_and_signature(aliased_function))
  expect_snapshot(extract_comments_and_signature(indented_comments))
  expect_snapshot(extract_comments_and_signature(no_srcfile))
})

test_that("basic signature extraction works", {
  sys.source(test_path("tools-def.R"), environment(), keep.source = TRUE)
  expect_snapshot(extract_comments_and_signature(no_roxygen_comments))
})

test_that("checks its inputs", {
  expect_snapshot(error = TRUE, {
    create_tool_def(print, model = "gpt-4", chat = chat_google_gemini())
    create_tool_def(print, chat = 1)
  })
})

test_that("model is deprecated", {
  mock <- mocked_chat("response")
  local_mocked_bindings(chat_openai = function(...) mock)

  expect_snapshot(
    . <- create_tool_def(print, model = "gpt-4", echo = FALSE)
  )
})
