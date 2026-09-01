test_that("as_file_id() accepts an id string or a ContentUploaded", {
  expect_equal(as_file_id("file_123"), "file_123")
  expect_equal(
    as_file_id(ContentUploaded("file_123", "application/pdf")),
    "file_123"
  )
  expect_snapshot(error = TRUE, as_file_id(1))
})

test_that("uploaded files error for providers without support", {
  provider <- Provider(name = "test", base_url = "https://example.com")
  expect_snapshot(
    error = TRUE,
    as_json(provider, ContentUploaded("file_123", "application/pdf"))
  )
})
