test_that("only the Gemini API supports batch requests", {
  gemini <- chat_google_gemini_test()$get_provider()
  vertex <- ProviderGoogle(
    name = "Google/Vertex",
    base_url = "https://aiplatform.googleapis.com/v1/",
    credentials = function() list()
  )
  expect_equal(has_batch_support(gemini), TRUE)
  expect_equal(has_batch_support(vertex), FALSE)
})
