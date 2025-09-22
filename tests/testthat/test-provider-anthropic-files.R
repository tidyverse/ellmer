test_that("can upload a file an ask questions about it", {
  vcr::local_cassette("anthropic-upload-file")
  upload <- anthropic_upload_file(test_path("apples.pdf"))

  chat <- chat_anthropic_test(beta_headers = "files-api-2025-04-14")
  response <- chat$chat("What's the title of this document?", upload)
  expect_match(response, "Apples are tasty")
  expect_match(chat$chat("What apple is not tasty?"), "red delicious")
})
