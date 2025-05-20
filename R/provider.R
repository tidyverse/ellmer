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
#' @param name Name of the provider.
#' @param model Name of the model.
#' @param base_url The base URL for the API.
#' @param params A list of standard parameters created by [params()].
#' @param extra_args Arbitrary extra arguments to be included in the request body.
#' @return An S7 Provider object.
#' @examples
#' Provider(
#'   name = "CoolModels",
#'   model = "my_model",
#'   base_url = "https://cool-models.com"
#' )
Provider <- new_class(
  "Provider",
  properties = list(
    name = prop_string(),
    model = prop_string(),
    base_url = prop_string(),
    params = class_list,
    extra_args = class_list
  )
)

test_provider <- function(name = "", model = "", base_url = "", ...) {
  Provider(name = name, model = model, base_url = base_url, ...)
}

# Create a request------------------------------------

base_request <- new_generic("base_request", "provider", function(provider) {
  S7_dispatch()
})

base_request_error <- new_generic(
  "base_request_error",
  "provider",
  function(provider, req) {
    S7_dispatch()
  }
)

chat_request <- new_generic(
  "chat_request",
  "provider",
  function(
    provider,
    stream = TRUE,
    turns = list(),
    tools = list(),
    type = NULL
  ) {
    S7_dispatch()
  }
)

method(chat_request, Provider) <- function(
  provider,
  stream = TRUE,
  turns = list(),
  tools = list(),
  type = NULL
) {
  req <- base_request(provider)
  req <- req_url_path_append(req, chat_path(provider))

  body <- chat_body(
    provider = provider,
    stream = stream,
    turns = turns,
    tools = tools,
    type = type
  )
  body <- modify_list(body, provider@extra_args)
  req <- req_body_json(req, body)

  req
}

chat_body <- new_generic(
  "chat_body",
  "provider",
  function(
    provider,
    stream = TRUE,
    turns = list(),
    tools = list(),
    type = NULL
  ) {
    S7_dispatch()
  }
)

chat_path <- new_generic("chat_path", "provider", function(provider) {
  S7_dispatch()
})

chat_resp_stream <- new_generic(
  "chat_resp_stream",
  "provider",
  function(provider, resp) {
    S7_dispatch()
  }
)
method(chat_resp_stream, Provider) <- function(provider, resp) {
  resp_stream_sse(resp)
}

chat_params <- new_generic(
  "chat_params",
  "provider",
  function(provider, params) {
    S7_dispatch()
  }
)

# Extract data from streaming results ------------------------------------

stream_parse <- new_generic(
  "stream_parse",
  "provider",
  function(provider, event) {
    S7_dispatch()
  }
)
stream_text <- new_generic(
  "stream_text",
  "provider",
  function(provider, event) {
    S7_dispatch()
  }
)
stream_merge_chunks <- new_generic(
  "stream_merge_chunks",
  "provider",
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

# Batch AI ---------------------------------------------------------------

# Does the provider support batch uploads?
has_batch_support <- new_generic(
  "has_batch_support",
  "provider",
  function(provider) {
    S7_dispatch()
  }
)
method(has_batch_support, Provider) <- function(provider) {
  FALSE
}

# Submit a batch, return an object "batch" object that will be passed to
# batch_poll() and batch_retrieve()
batch_submit <- new_generic(
  "batch_submit",
  "provider",
  function(provider, conversations, type = NULL) {
    S7_dispatch()
  }
)

# Get batch status. Returns an opaque list.
batch_poll <- new_generic(
  "batch_poll",
  "provider",
  function(provider, batch) {
    S7_dispatch()
  }
)

# Given batch status, return a standardised list:
# * working - TRUE/FALSE
# * n_processing = number of requests still processing
# * n_succeeded = number of requests that succeeded
# * n_failed = number of requests that failed
batch_status <- new_generic(
  "batch_status",
  "provider",
  function(provider, batch) {
    S7_dispatch()
  }
)

# Download batched results
batch_retrieve <- new_generic(
  "batch_retrieve",
  "provider",
  function(provider, batch) {
    S7_dispatch()
  }
)

# Process a single result. Returns either a turn or NULL, if the turn
# did not succeed
batch_result_turn <- new_generic(
  "batch_result_turn",
  "provider",
  function(provider, result, has_type = FALSE) {
    S7_dispatch()
  }
)

# Pricing ---------------------------------------------------------------------

standardise_model <- new_generic(
  "standardise_model",
  "provider",
  function(provider, model) {
    S7_dispatch()
  }
)

method(standardise_model, Provider) <- function(provider, model) {
  model
}
