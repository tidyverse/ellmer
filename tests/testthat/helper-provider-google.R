# A provider for the generateContent API, which is used by chat_google_vertex()
# and by batch requests and token counting for chat_google_gemini(). Chat
# requests for chat_google_gemini() use the Interactions API instead.
google_test_provider <- function() {
  ProviderGoogle(
    name = "Google/Vertex",
    base_url = "https://aiplatform.googleapis.com/v1/",
    credentials = function() list()
  )
}
