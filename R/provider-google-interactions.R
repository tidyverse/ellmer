#' @include provider-google.R
#' @include tools-built-in.R
NULL

ProviderGoogleGemini <- new_class(
  "ProviderGoogleGemini",
  parent = ProviderGoogle
)

# Base request -----------------------------------------------------------------

method(base_request, ProviderGoogleGemini) <- function(provider) {
  req <- request(provider@base_url)
  req <- ellmer_req_credentials(req, provider@credentials(), "x-goog-api-key")
  req <- ellmer_req_robustify(req, is_transient = gemini_is_transient)
  req <- ellmer_req_user_agent(req)
  req <- req_error(req, body = gemini_error_body)
  req
}

# https://ai.google.dev/gemini-api/docs/api-errors
gemini_is_transient <- function(resp) {
  status <- resp_status(resp)
  if (status == 429) {
    # Also used when the daily quota is exhausted, where retrying can't help
    !identical(gemini_error(resp)$code, "quota_exceeded")
  } else {
    status == 503
  }
}

gemini_error_body <- function(resp) {
  error <- gemini_error(resp)
  if (!is.null(error)) {
    paste0(error$message, " [", error$code, "]")
  }
}

gemini_error <- function(resp) {
  if (identical(resp_content_type(resp), "application/json")) {
    resp_body_json(resp)$error
  }
}

# Chat -------------------------------------------------------------------------

# https://ai.google.dev/api/interactions-api
method(chat_request, ProviderGoogleGemini) <- function(
  provider,
  model,
  stream = TRUE,
  turns = list(),
  tools = list(),
  type = NULL
) {
  req <- base_request(provider)
  req <- req_url_path_append(req, "interactions")
  if (stream) {
    req <- req_url_query(req, alt = "sse")
  }

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

method(chat_body, ProviderGoogleGemini) <- function(
  provider,
  model,
  stream = TRUE,
  turns = list(),
  tools = list(),
  type = NULL
) {
  if (length(turns) >= 1 && is_system_turn(turns[[1]])) {
    system <- turns[[1]]@text
  } else {
    system <- NULL
  }

  generation_config <- chat_params(provider, model@params)
  if (has_name(generation_config, "thinking_level")) {
    generation_config$thinking_summaries <- "auto"
  }

  if (!is.null(type)) {
    response_format <- list(
      type = "text",
      mime_type = "application/json",
      schema = as_json(provider, type)
    )
  } else {
    response_format <- NULL
  }

  compact(list(
    model = model@name,
    store = FALSE,
    stream = stream,
    system_instruction = system,
    input = unlist(as_json(provider, turns), recursive = FALSE),
    tools = chat_body_tools(provider, tools),
    response_format = response_format,
    generation_config = generation_config
  ))
}

method(chat_params, ProviderGoogleGemini) <- function(provider, params) {
  standardise_params(
    params,
    c(
      temperature = "temperature",
      top_p = "top_p",
      top_k = "top_k",
      seed = "seed",
      max_output_tokens = "max_tokens",
      stop_sequences = "stop_sequences",
      thinking_level = "reasoning_effort"
    )
  )
}

method(chat_body_tools, ProviderGoogleGemini) <- function(provider, tools) {
  as_json(provider, unname(tools))
}

# ellmer -> Interactions -------------------------------------------------------

# A turn becomes a list of steps. Text and media contents are grouped into a
# single user_input or model_output step; everything else (thoughts, tool
# calls, tool results) is already a step of its own.
method(as_json, list(ProviderGoogleGemini, Turn)) <- function(
  provider,
  x,
  ...
) {
  if (is_system_turn(x)) {
    return(NULL)
  }
  if (is_user_turn(x)) {
    x <- turn_contents_expand(x)
    type <- "user_input"
  } else {
    type <- "model_output"
  }
  gemini_steps(as_json(provider, x@contents, ...), type)
}

# `json` is the serialized contents of a turn: a mix of content blocks
# (text, image, ...) and steps (thought, function_call, ...). Consecutive
# content blocks are wrapped in a single step of `type`, e.g.
#
#   thought, text, text, function_call, text
#
# becomes
#
#   thought, model_output(text, text), function_call, model_output(text)
gemini_steps <- function(json, type) {
  is_block <- map_lgl(json, is_content_block)

  # Number the items so that each item gets a new number, unless it's a block
  # that follows another block, in which case it shares the previous number
  joins_previous <- is_block & c(FALSE, head(is_block, -1))
  groups <- unname(split(json, cumsum(!joins_previous)))

  # A group is now either a run of blocks (to be wrapped in a step) or a
  # single step (to be used as is)
  lapply(groups, function(group) {
    if (is_content_block(group[[1]])) {
      list(type = type, content = group)
    } else {
      group[[1]]
    }
  })
}

is_content_block <- function(x) {
  x$type %in% c("text", "image", "audio", "video", "document")
}

method(as_json, list(ProviderGoogleGemini, ToolDef)) <- function(
  provider,
  x,
  ...
) {
  compact(list(
    type = "function",
    name = x@name,
    description = x@description,
    parameters = as_json(provider, x@arguments, ...)
  ))
}

method(as_json, list(ProviderGoogleGemini, ToolBuiltIn)) <- function(
  provider,
  x,
  ...
) {
  list(type = names(x@json))
}

method(as_json, list(ProviderGoogleGemini, ContentText)) <- function(
  provider,
  x,
  ...
) {
  if (identical(x@text, "")) {
    NULL
  } else {
    list(type = "text", text = x@text)
  }
}

# Thoughts are replayed verbatim so their signatures are preserved
method(as_json, list(ProviderGoogleGemini, ContentThinking)) <- function(
  provider,
  x,
  ...
) {
  if (identical(x@extra$type, "thought")) {
    x@extra
  }
}

method(as_json, list(ProviderGoogleGemini, ContentImageInline)) <- function(
  provider,
  x,
  ...
) {
  list(type = "image", data = x@data, mime_type = x@type)
}

method(as_json, list(ProviderGoogleGemini, ContentImageRemote)) <- function(
  provider,
  x,
  ...
) {
  list(type = "image", uri = x@url)
}

method(as_json, list(ProviderGoogleGemini, ContentPDF)) <- function(
  provider,
  x,
  ...
) {
  list(type = "document", data = x@data, mime_type = x@type)
}

method(as_json, list(ProviderGoogleGemini, ContentDocument)) <- function(
  provider,
  x,
  ...
) {
  if (!is_text_document(x@mime_type)) {
    cli::cli_abort(c(
      "Gemini doesn't support {.str {x@mime_type}} documents.",
      i = "Convert the document to plain text or PDF first."
    ))
  }
  list(type = "document", data = x@data, mime_type = x@mime_type)
}

method(as_json, list(ProviderGoogleGemini, ContentUploaded)) <- function(
  provider,
  x,
  ...
) {
  type <- switch(
    sub("/.*", "", x@mime_type),
    image = "image",
    audio = "audio",
    video = "video",
    "document"
  )
  list(type = type, uri = x@uri, mime_type = x@mime_type)
}

method(as_json, list(ProviderGoogleGemini, ContentToolRequest)) <- function(
  provider,
  x,
  ...
) {
  arguments <- x@arguments
  if (length(arguments) == 0) {
    # Must serialize as {} rather than []
    arguments <- set_names(list())
  }
  list(
    type = "function_call",
    id = x@id,
    name = x@name,
    arguments = arguments
  )
}

method(as_json, list(ProviderGoogleGemini, ContentToolResult)) <- function(
  provider,
  x,
  ...
) {
  list(
    type = "function_result",
    call_id = x@request@id,
    name = x@request@name,
    result = list(list(type = "text", text = tool_string(x))),
    is_error = tool_errored(x)
  )
}

# Batched requests -------------------------------------------------------------

method(has_batch_support, ProviderGoogleGemini) <- function(provider) {
  TRUE
}
