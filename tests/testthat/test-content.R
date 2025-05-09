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
  turn <- Turn(
    "user",
    contents = list(
      ContentText("User input."),
      ContentImageInline("image/png", "abcd123"),
      ContentImageRemote("https://example.com/image.jpg", detail = ""),
      ContentJson(list(a = 1:2, b = "apple")),
      ContentSql("SELECT * FROM mtcars"),
      ContentSuggestions(
        c(
          "What is the total quantity sold for each product last quarter?",
          "What is the average discount percentage for orders from the United States?",
          "What is the average price of products in the 'electronics' category?"
        )
      )
    )
  )

  expect_snapshot(cat(contents_text(turn)))
  expect_snapshot(cat(contents_markdown(turn)))

  turns <- list(
    turn,
    Turn("assistant", list(ContentText("Here's your answer.")))
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

test_that("ContentToolResult@error requires a string or an error condition", {
  expect_snapshot(error = TRUE, {
    ContentToolResult("id", error = TRUE)
    ContentToolResult("id", error = c("one", "two"))
  })
})
