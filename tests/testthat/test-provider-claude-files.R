test_that("file lifecycle works", {
  vcr::local_cassette("anthropic-files")
  test_file_lifecycle(chat_anthropic_test)
})

test_that("file_upload() validates expires_in_h", {
  provider <- chat_anthropic_test()$get_provider()
  path <- test_path("apples.pdf")
  expect_snapshot(error = TRUE, {
    file_upload(provider, path, expires_in_h = 0.5)
    file_upload(provider, path, expires_in_h = 91 * 24)
  })
})

test_that("claude_file_upload() is deprecated", {
  withr::local_options(lifecycle_verbosity = "warning")
  local_mocked_bindings(file_upload = function(...) invisible())
  expect_snapshot(. <- claude_file_upload(test_path("apples.pdf")))
})

test_that("deprecated claude_file_* wrappers still work end-to-end", {
  withr::local_options(lifecycle_verbosity = "quiet")
  vcr::local_cassette("anthropic-upload-file")
  # Avoid using an absolute path in form_file
  local_mocked_bindings(
    form_file = function(path, type) {
      structure(
        list(path = path, type = type, name = NULL),
        class = "form_file"
      )
    }
  )

  upload <- claude_file_upload(test_path("apples.pdf"))
  defer(claude_file_delete(upload@uri))

  chat <- chat_anthropic_test()
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
