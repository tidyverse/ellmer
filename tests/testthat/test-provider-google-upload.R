test_that("file operations error on Vertex", {
  provider <- ProviderGoogleGemini(
    name = "Google/Vertex",
    base_url = "https://us-central1-aiplatform.googleapis.com/v1beta1/",
    credentials = function() list()
  )
  expect_snapshot(error = TRUE, file_upload(provider, "apples.pdf"))
})

test_that("google_upload() works end-to-end", {
  vcr::local_cassette("google-upload")
  upload <- google_upload(test_path("apples.pdf"))

  chat <- chat_google_gemini_test()
  response <- chat$chat("What's the title of this document?", upload)
  expect_match(response, "Apples are tasty")
  expect_match(chat$chat("What apple is not tasty?"), "red delicious")
})
