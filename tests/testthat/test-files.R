test_that("as_file_id() accepts an id string or a ContentUploaded", {
  expect_equal(as_file_id("file_123"), "file_123")
  expect_equal(
    as_file_id(ContentUploaded("file_123", "application/pdf")),
    "file_123"
  )
  expect_snapshot(error = TRUE, as_file_id(1))
})

test_that("has_uploaded_content() detects uploads in turns", {
  text_turn <- UserTurn(list(ContentText("hello")))
  uploaded_turn <- UserTurn(list(ContentUploaded(
    "file_123",
    "application/pdf"
  )))
  expect_equal(has_uploaded_content(list(text_turn)), FALSE)
  expect_equal(has_uploaded_content(list(text_turn, uploaded_turn)), TRUE)
})
