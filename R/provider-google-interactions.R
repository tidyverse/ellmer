#' @include provider-google.R
NULL

# The Gemini Developer API uses the Interactions API; Vertex AI still uses
# generateContent, so the shared implementation lives in ProviderGoogle.
# https://ai.google.dev/api/interactions-api
ProviderGoogleGemini <- new_class(
  "ProviderGoogleGemini",
  parent = ProviderGoogle
)

# Batched requests -------------------------------------------------------------

method(has_batch_support, ProviderGoogleGemini) <- function(provider) {
  TRUE
}
