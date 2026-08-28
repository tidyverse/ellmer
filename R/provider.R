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
#' @param base_url The base URL for the API.
#' @param credentials A zero-argument function that returns the credentials
#'   to use for authentication. Can either return a string, representing an
#'   API key, or a named list of headers.
#' @param extra_headers Arbitrary extra headers to be added to the request.
#' @param model,params,extra_args `r lifecycle::badge("deprecated")` These now
#'   live on the [Model] object; use `chat$get_model_object()` instead.
#' @return An S7 Provider object.
#' @examples
#' Provider(
#'   name = "CoolModels",
#'   base_url = "https://cool-models.com"
#' )
Provider <- new_class(
  "Provider",
  properties = list(
    name = prop_string(),
    base_url = prop_string(),
    extra_headers = class_character,
    credentials = class_function | NULL,
    # Deprecated in 0.5.0: model details now live on Model. Remove in 0.6.0 (#1098).
    model = prop_deprecated("model", "name"),
    params = prop_deprecated("params", "params"),
    extra_args = prop_deprecated("extra_args", "extra_args")
  )
)

# Default S7 print calls every getter, which would trigger the deprecation
# warnings. Remove along with the deprecated properties (#1098).
method(print, Provider) <- function(x, ...) {
  names <- setdiff(prop_names(x), c("model", "params", "extra_args"))
  props <- set_names(lapply(names, \(name) prop(x, name)), names)
  cat("<", class(x)[[1]], ">\n", sep = "")
  str(
    props,
    no.list = TRUE,
    give.attr = FALSE,
    comp.str = "@ ",
    indent.str = " "
  )
  invisible(x)
}

test_provider <- function(name = "", base_url = "", ...) {
  Provider(name = name, base_url = base_url, ...)
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
    model,
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
  model,
  stream = TRUE,
  turns = list(),
  tools = list(),
  type = NULL
) {
  req <- base_request(provider)
  req <- req_url_path_append(req, chat_path(provider))

  body <- chat_body(
    provider = provider,
    model = model,
    stream = stream,
    turns = turns,
    tools = tools,
    type = type
  )
  body <- modify_list(body, model@extra_args)
  req <- req_body_json(req, body)
  req <- req_headers(req, !!!provider@extra_headers)

  req
}

chat_body_tools <- new_generic(
  "chat_body_tools",
  "provider",
  function(provider, tools) S7_dispatch()
)

method(chat_body_tools, Provider) <- function(provider, tools) {
  as_json(provider, unname(tools))
}

chat_body <- new_generic(
  "chat_body",
  "provider",
  function(
    provider,
    model,
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
stream_content <- new_generic(
  "stream_content",
  "provider",
  function(provider, event, completion = NULL) {
    S7_dispatch()
  }
)
stream_content_with_turns <- new_generic(
  "stream_content_with_turns",
  "provider",
  function(provider, event, completion = NULL, turns = list()) {
    S7_dispatch()
  }
)
method(stream_content_with_turns, Provider) <- function(
  provider,
  event,
  completion = NULL,
  turns = list()
) {
  stream_content(provider, event, completion)
}

stream_text <- function(provider, event) {
  contents <- stream_content(provider, event)
  contents <- keep(contents, is_stream_text_content)
  if (length(contents) == 0) {
    return(NULL)
  }
  paste0(map_chr(contents, content_text), collapse = "")
}

content_text <- function(content) {
  switch(
    class(content)[1],
    "ellmer::ContentThinking" = content@thinking,
    "ellmer::ContentText" = content@text,
    format(content)
  )
}

is_stream_text_content <- function(content) {
  S7_inherits(content, ContentText) ||
    S7_inherits(content, ContentThinking)
}
stream_merge_chunks <- new_generic(
  "stream_merge_chunks",
  "provider",
  function(provider, result, chunk) {
    S7_dispatch()
  }
)

# Extract data from non-streaming results --------------------------------------

value_turn <- new_generic(
  "value_turn",
  "provider",
  function(provider, model, result, has_type = FALSE) {
    S7_dispatch()
  }
)
value_turn_with_turns <- new_generic(
  "value_turn_with_turns",
  "provider",
  function(provider, model, result, has_type = FALSE, turns = list()) {
    S7_dispatch()
  }
)
method(value_turn_with_turns, Provider) <- function(
  provider,
  model,
  result,
  has_type = FALSE,
  turns = list()
) {
  value_turn(provider, model, result, has_type = has_type)
}

# Extract token counts from API response
# Returns a named list produced by token_usage()
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

# Convert to JSON
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

method(as_json, list(Provider, ContentCitation)) <- function(provider, x, ...) {
  NULL
}

method(as_json, list(Provider, ContentJson)) <- function(provider, x, ...) {
  if (!is.null(x@string)) {
    string <- x@string
  } else {
    string <- unclass(jsonlite::toJSON(x@data, auto_unbox = TRUE))
  }
  as_json(provider, ContentText(string), ...)
}

# Token counting -------------------------------------------------------------

count_tokens <- new_generic(
  "count_tokens",
  "provider",
  function(
    provider,
    model,
    ...,
    system_prompt = NULL,
    tools = list(),
    type = NULL
  ) {
    S7_dispatch()
  }
)

method(count_tokens, Provider) <- function(
  provider,
  model,
  ...,
  system_prompt = NULL,
  tools = list(),
  type = NULL
) {
  cli::cli_abort(
    "{provider@name} doesn't support token counting.",
    class = "not_implemented"
  )
}

# Models -------------------------------------------------------------------

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
  function(provider, model, conversations, type = NULL) {
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
  function(provider, model, result, has_type = FALSE) {
    S7_dispatch()
  }
)
