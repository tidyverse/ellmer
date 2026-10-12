test_that("can create document from path", {
  obj <- content_document_file(test_path("penguin_race.csv"))
  expect_s3_class(obj, "ellmer::ContentDocument")
  expect_equal(obj@mime_type, "text/csv")
  expect_equal(obj@filename, "penguin_race.csv")
  expect_null(obj@url)
})

test_that("infers mime type from extension", {
  expect_equal(
    guess_mime_type("notes.md", default = "text/plain"),
    "text/markdown"
  )
  expect_equal(
    guess_mime_type("data.json", default = "text/plain"),
    "application/json"
  )
  expect_equal(
    guess_mime_type("report.DOCX", default = "text/plain"),
    "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
  )
  # unknown extensions are assumed to be code/text
  expect_equal(
    guess_mime_type("script.R", default = "text/plain"),
    "text/plain"
  )
  expect_equal(
    guess_mime_type("example.com", default = "text/plain"),
    "text/plain"
  )
})

test_that("explicit mime_type overrides extension", {
  path <- withr::local_tempfile(fileext = ".bin", lines = "abc")
  obj <- content_document_file(path, mime_type = "application/octet-stream")
  expect_equal(obj@mime_type, "application/octet-stream")
})

test_that("pdfs are redirected to content_pdf_ functions", {
  path <- withr::local_tempfile(fileext = ".pdf", lines = "fake")
  expect_snapshot(error = TRUE, {
    content_document_file(path)
    content_document_url("https://example.com/report.pdf")
    content_document_file(
      test_path("penguin_race.csv"),
      mime_type = "application/pdf"
    )
  })
})

test_that("errors if file doesn't exist", {
  expect_snapshot(content_document_file("DOESNTEXIST"), error = TRUE)
})

test_that("can create document from data url", {
  data <- openssl::base64_encode("a,b\n1,2\n")
  obj <- content_document_url(paste0("data:text/csv;base64,", data))
  expect_s3_class(obj, "ellmer::ContentDocument")
  expect_equal(obj@mime_type, "text/csv")
  expect_equal(obj@data, data)
  expect_null(obj@url)
  expect_match(obj@filename, "\\.csv$")
})

test_that("generated filenames are distinct", {
  url <- "data:text/csv;base64,YQ=="
  names <- c(
    content_document_url(url)@filename,
    content_document_url(url)@filename
  )
  expect_length(unique(names), 2)
})

test_that("has useful format method", {
  obj <- content_document_file(test_path("penguin_race.csv"))
  expect_equal(format(obj), "<document penguin_race.csv (text/csv)>")
})
