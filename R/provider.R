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
#' @param credentials A zero-argument function that returns the credentials to use
#'   for authentication. Can either return a string, representing an API key,
#'   or a named list of headers.
#' @param extra_args Arbitrary extra arguments to be included in the request body.
#' @param extra_headers Arbitrary extra headers to be added to the request.
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
    extra_args = class_list,
    extra_headers = class_character,
    credentials = class_function | NULL
  )
)

test_provider <- function(name = "", model = "", base_url = "", ...) {
  Provider(name = name, model = model, base_url = base_url, ...)
}

# Create a request------------------------------------

#' Provider generics
#'
#' @description
#' These generics define the interface that provider subclasses can implement.
#' They are exported so that package developers creating custom providers
#' can register methods for their own provider classes.
#'
#' * `base_request()`: Build the base [httr2::request()] for the provider,
#'   including authentication and error handling.
#' * `base_request_error()`: Customize error handling for the provider's
#'   API responses.
#' * `chat_body()`: Build the JSON body for a chat API call.
#' * `chat_path()`: Return the URL path for the chat endpoint
#'   (e.g., `"/chat/completions"`).
#' * `chat_params()`: Map standardized [params()] to provider-specific
#'   API parameter names.
#' * `as_json()`: Convert ellmer objects ([Turn]s, [Content], [ToolDef]s,
#'   [Type]s) to the JSON structure expected by the provider's API.
#' * `models_list()`: List available models from the provider.
#'
#' ## Streaming
#'
#' * `chat_resp_stream()`: Set up a streaming response connection from
#'   an [httr2::response()]. The default uses [httr2::resp_stream_sse()].
#' * `stream_parse()`: Parse a single server-sent event into an R list.
#' * `stream_content()`: Extract a [Content] object from a parsed
#'   streaming event.
#' * `stream_merge_chunks()`: Merge a new streaming chunk into the
#'   accumulated result.
#'
#' ## Response processing
#'
#' * `value_turn()`: Convert a complete (non-streaming) API response
#'   into an [AssistantTurn].
#' * `value_tokens()`: Extract token usage counts from an API response.
#' * `value_finish_reason()`: Extract and standardize the finish reason
#'   from an API response.
#'
#' ## Batch
#'
#' * `has_batch_support()`: Return `TRUE` if the provider supports batch
#'   requests.
#' * `batch_submit()`: Submit a batch of conversations.
#' * `batch_poll()`: Poll for batch status.
#' * `batch_status()`: Extract standardized status from a batch poll
#'   result.
#' * `batch_retrieve()`: Download batch results.
#' * `batch_result_turn()`: Process a single batch result into a turn.
#'
#' @param provider A [Provider] object.
#' @param req An [httr2::request()] object.
#' @param resp An [httr2::response()] object.
#' @param stream Whether to stream the response.
#' @param turns A list of [Turn] objects.
#' @param tools A list of [ToolDef] objects.
#' @param type A [Type] object for structured output, or `NULL`.
#' @param params A list of parameters, usually from [params()].
#' @param x An object to convert to JSON.
#' @param ... Additional arguments passed to methods.
#' @param event A server-sent event to parse or extract content from.
#' @param result The accumulated result from merged streaming chunks,
#'   or a single batch result.
#' @param chunk A new streaming chunk to merge.
#' @param json The parsed JSON response body.
#' @param has_type Whether structured output was requested.
#' @param conversations A list of conversations to submit as a batch.
#' @param batch An opaque batch object returned by `batch_submit()` or
#'   `batch_poll()`.
#' @name provider-generics
#' @return Varies by generic.
#' @export
base_request <- new_generic("base_request", "provider", function(provider) {
  S7_dispatch()
})

#' @rdname provider-generics
#' @export
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
  req <- req_headers(req, !!!provider@extra_headers)

  req
}

#' @rdname provider-generics
#' @export
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

#' @rdname provider-generics
#' @export
chat_path <- new_generic("chat_path", "provider", function(provider) {
  S7_dispatch()
})

#' @rdname provider-generics
#' @export
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

#' @rdname provider-generics
#' @export
chat_params <- new_generic(
  "chat_params",
  "provider",
  function(provider, params) {
    S7_dispatch()
  }
)

# Extract data from streaming results ------------------------------------

#' @rdname provider-generics
#' @export
stream_parse <- new_generic(
  "stream_parse",
  "provider",
  function(provider, event) {
    S7_dispatch()
  }
)
#' @rdname provider-generics
#' @export
stream_content <- new_generic(
  "stream_content",
  "provider",
  function(provider, event) {
    S7_dispatch()
  }
)

stream_text <- function(provider, event) {
  content <- stream_content(provider, event)
  if (is.null(content)) {
    return(NULL)
  }
  content_text(content)
}

content_text <- function(content) {
  switch(
    class(content)[1],
    "ellmer::ContentThinking" = content@thinking,
    "ellmer::ContentText" = content@text,
    format(content)
  )
}
#' @rdname provider-generics
#' @export
stream_merge_chunks <- new_generic(
  "stream_merge_chunks",
  "provider",
  function(provider, result, chunk) {
    S7_dispatch()
  }
)

# Extract data from non-streaming results --------------------------------------

#' @rdname provider-generics
#' @export
value_turn <- new_generic("value_turn", "provider")

#' @rdname provider-generics
#' @export
value_tokens <- new_generic(
  "value_tokens",
  "provider",
  function(provider, json) {
    S7_dispatch()
  }
)
method(value_tokens, Provider) <- function(provider, json) {
  tokens()
}

#' @rdname provider-generics
#' @export
value_finish_reason <- new_generic(
  "value_finish_reason",
  "provider",
  function(provider, result) {
    S7_dispatch()
  }
)
method(value_finish_reason, Provider) <- function(provider, result) {
  NA_character_
}

#' @rdname provider-generics
#' @export
as_json <- new_generic(
  "as_json",
  c("provider", "x"),
  function(provider, x, ...) {
    S7_dispatch()
  }
)

method(as_json, list(Provider, class_list)) <- function(provider, x, ...) {
  compact(lapply(x, as_json, provider = provider, ...))
}

method(as_json, list(Provider, ContentJson)) <- function(provider, x, ...) {
  if (!is.null(x@string)) {
    string <- x@string
  } else {
    string <- unclass(jsonlite::toJSON(x@data, auto_unbox = TRUE))
  }
  as_json(provider, ContentText(string), ...)
}

# Models -------------------------------------------------------------------

#' @rdname provider-generics
#' @export
models_list <- new_generic("models_list", "provider", function(provider) {
  S7_dispatch()
})

method(models_list, Provider) <- function(provider) {
  cli::cli_abort(
    "{.arg provider} doesn't support model listing.",
    class = "not_implemented"
  )
}

method(models_list, new_S3_class("Chat")) <- function(provider) {
  models_list(provider$get_provider())
}

# Batch AI ---------------------------------------------------------------

#' @rdname provider-generics
#' @export
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

#' @rdname provider-generics
#' @export
batch_submit <- new_generic(
  "batch_submit",
  "provider",
  function(provider, conversations, type = NULL) {
    S7_dispatch()
  }
)

#' @rdname provider-generics
#' @export
batch_poll <- new_generic(
  "batch_poll",
  "provider",
  function(provider, batch) {
    S7_dispatch()
  }
)

#' @rdname provider-generics
#' @export
batch_status <- new_generic(
  "batch_status",
  "provider",
  function(provider, batch) {
    S7_dispatch()
  }
)

#' @rdname provider-generics
#' @export
batch_retrieve <- new_generic(
  "batch_retrieve",
  "provider",
  function(provider, batch) {
    S7_dispatch()
  }
)

#' @rdname provider-generics
#' @export
batch_result_turn <- new_generic(
  "batch_result_turn",
  "provider",
  function(provider, result, has_type = FALSE) {
    S7_dispatch()
  }
)
