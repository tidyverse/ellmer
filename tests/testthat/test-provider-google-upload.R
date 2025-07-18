test_that("can upload a file an ask questions about it", {
  vcr::local_cassette("google-upload")
  upload <- google_upload(test_path("apples.pdf"))

  chat <- chat_google_gemini_test()
  response <- chat$chat("What's the title of this document?", upload)
  expect_match(response, "Apples are tasty")
  expect_match(chat$chat("What apple is not tasty?"), "red delicious")
})
