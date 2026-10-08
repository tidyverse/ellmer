# For offline tests of the generateContent format
google_generate_content_test_provider <- function() {
  ProviderGoogleGenerateContent(
    name = "Google/Gemini",
    base_url = "https://generativelanguage.googleapis.com/v1beta/",
    credentials = function() list()
  )
}
