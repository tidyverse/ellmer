test_that("can create pdf from path", {
  obj <- content_pdf_file(test_path("apples.pdf"))
  expect_s3_class(obj, "ellmer::ContentPDF")
})
