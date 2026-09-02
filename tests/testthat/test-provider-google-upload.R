test_that("file operations error on Vertex", {
  provider <- ProviderGoogleGemini(
    name = "Google/Vertex",
    base_url = "https://us-central1-aiplatform.googleapis.com/v1beta1/",
    credentials = function() list()
  )
  expect_snapshot(error = TRUE, file_upload(provider, "apples.pdf"))
})

test_that("file_upload() rejects expires_in_h other than 48 hours", {
  provider <- chat_google_gemini_test()$get_provider()
  path <- test_path("apples.pdf")
  expect_snapshot(error = TRUE, {
    file_upload(provider, path, expires_in_h = 1)
    file_upload(provider, path, expires_in_h = Inf)
  })
})

test_that("google_upload() is deprecated", {
  withr::local_options(lifecycle_verbosity = "warning")
  local_mocked_bindings(file_upload = function(...) invisible())
  expect_snapshot(. <- google_upload(test_path("apples.pdf")))
})

test_that("deprecated google_upload() still works end-to-end", {
  withr::local_options(lifecycle_verbosity = "quiet")
  vcr::local_cassette("google-upload")
  upload <- google_upload(test_path("apples.pdf"))

  chat <- chat_google_gemini_test()
  response <- chat$chat("What's the title of this document?", upload)
  expect_match(response, "Apples are tasty")
  expect_match(chat$chat("What apple is not tasty?"), "red delicious")
})
