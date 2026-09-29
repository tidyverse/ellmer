# The Gemini test provider as a generateContent provider: what
# chat_google_gemini() uses for batch requests and token counting, and the
# same class chat_google_vertex() uses for everything. Use
# chat_google_gemini_test()$get_provider() for the Interactions API.
google_gemini_test_provider <- function() {
  ProviderGoogleGenerateContent(
    name = "Google/Gemini",
    base_url = "https://generativelanguage.googleapis.com/v1beta/",
    credentials = function() list()
  )
}
