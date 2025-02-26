#' @include content.R
NULL

#' A chatbot provider
#'
#' A Provider captures the details of one chatbot service/API. This captures
#' how the API works, not the details of the underlying large language model.
#' Different providers might offer the same (open source) model behind a
#' different API.
#'
#' To add support for a new backend, you will need to subclass `Provider`
#' (adding any additional fields that your provider needs) and then implement
#' the various generics that control the behavior of each provider.
#'
#' @export
#' @param base_url The base URL for the API.
#' @param extra_args Arbitrary extra arguments to be included in the request body.
#' @return An S7 Provider object.
#' @examples
#' Provider(base_url = "https://cool-models.com")
Provider <- new_class(
  "Provider",
  properties = list(
    base_url = prop_string(),
    extra_args = class_list
  )
)

# Create a request------------------------------------

base_request <- new_generic("base_request", "provider",
  function(provider) {
    S7_dispatch()
  }
)

chat_request <- new_generic("chat_request", "provider",
  function(provider, stream = TRUE, turns = list(), tools = list(), type = NULL) {
    S7_dispatch()
  }
)

chat_body <- new_generic(
  "chat_body",
  "provider",
  function(provider, stream = TRUE, turns = list(), tools = list(), type = NULL) {
    S7_dispatch()
  }
)


chat_resp_stream <- new_generic("chat_resp_stream", "provider",
  function(provider, resp) {
    S7_dispatch()
  }
)
method(chat_resp_stream, Provider) <- function(provider, resp) {
  resp_stream_sse(resp)
}

# Extract data from streaming results ------------------------------------

stream_parse <- new_generic("stream_parse", "provider",
  function(provider, event) {
    S7_dispatch()
  }
)
stream_text <- new_generic("stream_text", "provider",
  function(provider, event) {
    S7_dispatch()
  }
)
stream_merge_chunks <- new_generic("stream_merge_chunks", "provider",
  function(provider, result, chunk) {
    S7_dispatch()
  }
)

# Extract data from non-streaming results --------------------------------------

value_turn <- new_generic("value_turn", "provider")

# Convert to JSON
as_json <- new_generic("as_json", c("provider", "x"))

method(as_json, list(Provider, class_list)) <- function(provider, x) {
  compact(lapply(x, as_json, provider = provider))
}

method(as_json, list(Provider, ContentJson)) <- function(provider, x) {
  as_json(provider, ContentText("<structured data/>"))
}

# Batch API ---------------------------------------------------------------

has_batch_support <- new_generic(
  "has_batch_support",
  "provider",
  function(provider) {
    S7_dispatch()
  }
)
method(has_batch_support, class_any) <- function(provider) {
  FALSE
}

batch_submit <- new_generic(
  "batch_submit",
  "provider",
  function(provider, turns, type = NULL) {
    S7_dispatch()
  }
)

batch_poll <- new_generic(
  "batch_poll",
  "provider",
  function(provider, batch) {
    S7_dispatch()
  }
)

batch_retrieve <- new_generic(
  "batch_retrieve",
  "provider",
  function(provider, batch) {
    S7_dispatch()
  }
)

batch_info <- new_generic(
  "batch_info",
  "provider",
  function(provider, batch) {
    S7_dispatch()
  }
)

batch_result_ok <- new_generic(
  "batch_result_ok",
  "provider",
  function(provider, result) {
    S7_dispatch()
  }
)
batch_result_turn <- new_generic(
  "batch_result_turn",
  "provider",
  function(provider, result, has_type = FALSE) {
    S7_dispatch()
  }
)
