test_that("end-to-end test of all functions", {
  vcr::local_cassette("anthropic-upload-file", record = "all")

  upload <- claude_file_upload(test_path("apples.pdf"))
  defer(claude_file_delete(upload@uri))

  chat <- chat_anthropic_test(beta_headers = "files-api-2025-04-14")
  response <- chat$chat("What's the title of this document?", upload)
  expect_match(response, "Apples are tasty")
  expect_match(
    chat$chat("What apple is not tasty? State only the name"),
    "red delicious",
    ignore.case = TRUE
  )

  # Can't download uploaded files, but at least tests that request succeeds
  path <- withr::local_tempfile()
  expect_error(claude_file_download(upload@uri, path), "not downloadable")

  files <- claude_file_list()
  expect_true("apples.pdf" %in% files$filename)
})
