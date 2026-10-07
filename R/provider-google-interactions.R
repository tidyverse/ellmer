#' @include provider-google.R
#' @include tools-built-in.R
NULL

ProviderGoogleInteractions <- new_class(
  "ProviderGoogleInteractions",
  parent = ProviderGoogle
)

# Base request -----------------------------------------------------------------

method(base_request, ProviderGoogleInteractions) <- function(provider) {
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
    body <- resp_body_json(resp)
    # Errors from the API gateway (e.g. a bad API key) are wrapped in an array
    if (!is_named(body) && length(body) > 0) {
      body <- body[[1]]
    }
    body$error
  }
}

# Chat -------------------------------------------------------------------------

# https://ai.google.dev/api/interactions-api
method(chat_request, ProviderGoogleInteractions) <- function(
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

method(chat_body, ProviderGoogleInteractions) <- function(
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

method(chat_params, ProviderGoogleInteractions) <- function(provider, params) {
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

method(chat_body_tools, ProviderGoogleInteractions) <- function(
  provider,
  tools
) {
  as_json(provider, unname(tools))
}

# ellmer -> Interactions -------------------------------------------------------

# A turn becomes a list of steps. Text and media contents are grouped into a
# single user_input or model_output step; everything else (thoughts, tool
# calls, tool results) is already a step of its own.
method(as_json, list(ProviderGoogleInteractions, Turn)) <- function(
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

# Consecutive content blocks are wrapped in a single step of `type`, e.g.
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

method(as_json, list(ProviderGoogleInteractions, ToolDef)) <- function(
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

method(as_json, list(ProviderGoogleInteractions, ToolBuiltIn)) <- function(
  provider,
  x,
  ...
) {
  list(type = names(x@json))
}

method(as_json, list(ProviderGoogleInteractions, ContentText)) <- function(
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
method(as_json, list(ProviderGoogleInteractions, ContentThinking)) <- function(
  provider,
  x,
  ...
) {
  if (identical(x@extra$type, "thought")) {
    x@extra
  }
}

method(
  as_json,
  list(ProviderGoogleInteractions, ContentImageInline)
) <- function(
  provider,
  x,
  ...
) {
  list(type = "image", data = x@data, mime_type = x@type)
}

method(
  as_json,
  list(ProviderGoogleInteractions, ContentImageRemote)
) <- function(
  provider,
  x,
  ...
) {
  # The API needs the type up front, so it's guessed from the URL's extension
  mime_type <- guess_mime_type(sub("[?#].*$", "", x@url), default = NA)
  if (is.na(mime_type) || !startsWith(mime_type, "image/")) {
    cli::cli_abort(c(
      "Can't guess the type of the image at {.url {x@url}} from its URL.",
      i = "Download the image and use {.fn content_image_file} instead."
    ))
  }
  list(type = "image", uri = x@url, mime_type = mime_type)
}

method(as_json, list(ProviderGoogleInteractions, ContentPDF)) <- function(
  provider,
  x,
  ...
) {
  list(type = "document", data = x@data, mime_type = x@type)
}

method(as_json, list(ProviderGoogleInteractions, ContentDocument)) <- function(
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

method(as_json, list(ProviderGoogleInteractions, ContentUploaded)) <- function(
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

method(
  as_json,
  list(ProviderGoogleInteractions, ContentToolRequest)
) <- function(
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

method(
  as_json,
  list(ProviderGoogleInteractions, ContentToolResult)
) <- function(
  provider,
  x,
  ...
) {
  list(
    type = "function_result",
    call_id = x@request@id,
    name = x@request@name,
    # A plain string is accepted by every model; the content-block form is
    # rejected by older ones (e.g. gemini-2.5-flash)
    result = tool_string(x),
    is_error = tool_errored(x)
  )
}

# Built-in tool activity is replayed verbatim from the raw step, when it came
# from this API (other providers' annotations also have a `type`)
gemini_replay_step <- function(provider, x, ...) {
  types <- c(
    "google_search_call",
    "google_search_result",
    "url_context_call",
    "url_context_result"
  )
  if (isTRUE(x@extra$type %in% types)) {
    x@extra
  }
}
method(as_json, list(ProviderGoogleInteractions, ContentToolRequestSearch)) <-
  gemini_replay_step
method(as_json, list(ProviderGoogleInteractions, ContentToolResponseSearch)) <-
  gemini_replay_step
method(as_json, list(ProviderGoogleInteractions, ContentToolRequestFetch)) <-
  gemini_replay_step
method(as_json, list(ProviderGoogleInteractions, ContentToolResponseFetch)) <-
  gemini_replay_step

# Interactions -> ellmer -------------------------------------------------------

method(value_turn, ProviderGoogleInteractions) <- function(
  provider,
  model,
  result,
  has_type = FALSE
) {
  contents <- gemini_step_contents(result$steps, has_type)

  tokens <- value_tokens(provider, result)
  cost <- get_token_cost(provider@name, model@name, tokens)

  AssistantTurn(
    contents,
    json = result,
    tokens = unlist(tokens),
    cost = cost,
    finish_reason = value_finish_reason(provider, result)
  )
}

gemini_step_contents <- function(steps, has_type = FALSE) {
  # The search result step only contains a widget for search suggestions;
  # the sources come from the citations in the answer
  sources <- gemini_web_sources(steps)

  contents <- list_c(lapply(steps, gemini_step_content, sources, has_type))
  # list_c() of nothing is NULL
  contents %||% list()
}

# Each step becomes a list of contents, since a single built-in tool step can
# cover several queries or URLs
gemini_step_content <- function(step, sources, has_type = FALSE) {
  type <- step$type
  if (type == "thought") {
    return(list(gemini_thinking(step)))
  }
  if (type == "model_output") {
    return(gemini_output_contents(step, has_type))
  }
  if (type == "function_call") {
    return(list(gemini_tool_request(step)))
  }
  if (type == "google_search_call") {
    return(gemini_replayed(
      step$arguments$queries,
      step,
      \(query, extra) ContentToolRequestSearch(query = query, extra = extra)
    ))
  }
  if (type == "google_search_result") {
    return(list(ContentToolResponseSearch(sources = sources, extra = step)))
  }
  if (type == "url_context_call") {
    return(gemini_replayed(
      step$arguments$urls,
      step,
      \(url, extra) ContentToolRequestFetch(url = url, extra = extra)
    ))
  }
  if (type == "url_context_result") {
    return(gemini_replayed(
      step$result,
      step,
      function(result, extra) {
        status <- if (identical(result$status, "success")) {
          "success"
        } else {
          "error"
        }
        ContentToolResponseFetch(
          url = result$url,
          status = status,
          extra = extra
        )
      }
    ))
  }
  cli::cli_abort("Unknown step type {.str {type}}.", .internal = TRUE)
}

gemini_thinking <- function(step) {
  summary <- step$summary %||% list()
  thinking <- paste0(map_chr(summary, "[[", "text"), collapse = "")
  ContentThinking(thinking = thinking, extra = step)
}

gemini_output_contents <- function(step, has_type = FALSE) {
  list_c(lapply(step$content, function(content) {
    if (content$type == "text") {
      if (has_type) {
        list(ContentJson(string = content$text))
      } else {
        c(list(ContentText(content$text)), gemini_citations(content))
      }
    } else if (content$type == "image") {
      list(ContentImageInline(type = content$mime_type, data = content$data))
    } else {
      cli::cli_abort(
        "Unknown content type {.str {content$type}}.",
        .internal = TRUE
      )
    }
  }))
}

gemini_citations <- function(content) {
  annotations <- gemini_url_citations(content$annotations)
  lapply(annotations, function(annotation) {
    ContentCitation(
      source = WebSource(url = annotation$url, title = annotation$title),
      grounded_span = substr_bytes(
        content$text,
        annotation$start_index %||% 0,
        annotation$end_index
      ),
      extra = annotation
    )
  })
}

# Citation indices are zero-based byte offsets, with `end` exclusive
substr_bytes <- function(text, start, end) {
  bytes <- charToRaw(enc2utf8(text))
  out <- rawToChar(bytes[seq2(start + 1, min(end, length(bytes)))])
  Encoding(out) <- "UTF-8"
  out
}

gemini_url_citations <- function(annotations) {
  keep(annotations %||% list(), function(annotation) {
    identical(annotation$type, "url_citation")
  })
}

gemini_web_sources <- function(steps) {
  outputs <- keep(steps, function(step) step$type == "model_output")
  contents <- list_c(lapply(outputs, "[[", "content"))
  annotations <- list_c(lapply(contents, function(content) {
    gemini_url_citations(content$annotations)
  }))
  annotations <- annotations[!duplicated(map_chr(annotations, "[[", "url"))]
  lapply(annotations, function(annotation) {
    WebSource(url = annotation$url, title = annotation$title)
  })
}

gemini_tool_request <- function(step) {
  arguments <- step$arguments
  if (is.character(arguments)) {
    # Streamed arguments arrive as a JSON string
    arguments <- jsonlite::parse_json(arguments)
  }
  ContentToolRequest(step$id, step$name, arguments)
}

# A built-in tool step may cover several queries or URLs, but must be replayed
# exactly once, so only the first content carries the raw step in `extra`
gemini_replayed <- function(items, step, make) {
  lapply(seq_along(items), function(i) {
    make(items[[i]], if (i == 1) step)
  })
}

method(value_tokens, ProviderGoogleInteractions) <- function(provider, json) {
  usage <- json$usage
  # total_input_tokens includes cached tokens; total_tokens also includes
  # thinking and tool use, which we count as output
  input <- usage$total_input_tokens %||% 0
  cached <- usage$total_cached_tokens %||% 0
  total <- usage$total_tokens %||% 0

  tokens(
    input = input - cached,
    output = total - input,
    cached_input = cached
  )
}

method(value_finish_reason, ProviderGoogleInteractions) <- function(
  provider,
  result
) {
  status <- result$status
  if (is.null(status)) {
    return(NA_character_)
  }
  switch(
    status,
    completed = "success",
    requires_action = "tool_use",
    incomplete = "max_tokens",
    I(status)
  )
}

# Streaming --------------------------------------------------------------------

# https://ai.google.dev/gemini-api/docs/streaming
method(stream_parse, ProviderGoogleInteractions) <- function(provider, event) {
  if (is.null(event) || identical(event$data, "[DONE]")) {
    NULL
  } else {
    jsonlite::parse_json(event$data)
  }
}

# Rebuilds the same structure as a non-streaming response (`status`, `usage`,
# `steps`), so that value_turn() can be shared. Steps are keyed by `index`.
method(stream_merge_chunks, ProviderGoogleInteractions) <- function(
  provider,
  result,
  chunk
) {
  type <- chunk$event_type
  if (type == "interaction.created") {
    result <- chunk$interaction
    result$steps <- list()
    return(result)
  }
  if (type == "step.start") {
    result$steps[[chunk$index + 1]] <- chunk$step
    return(result)
  }
  if (type == "step.delta") {
    i <- chunk$index + 1
    result$steps[[i]] <- gemini_merge_delta(result$steps[[i]], chunk$delta)
    return(result)
  }
  if (type == "interaction.completed") {
    return(modify_list(result, chunk$interaction))
  }
  if (type == "error") {
    cli::cli_abort(c(
      "Request failed ({chunk$error$code})",
      "{chunk$error$message}"
    ))
  }
  result
}

gemini_merge_delta <- function(step, delta) {
  if (is_content_block(delta) && !identical(delta$type, "text")) {
    step$content <- c(step$content, list(delta))
    return(step)
  }

  type <- delta$type
  if (type == "text") {
    content <- step$content
    n <- length(content)
    if (n > 0 && identical(content[[n]]$type, "text")) {
      content[[n]]$text <- paste0(content[[n]]$text, delta$text)
    } else {
      content[[n + 1]] <- delta
    }
    step$content <- content
    return(step)
  }
  if (type == "text_annotation_delta") {
    n <- length(step$content)
    step$content[[n]]$annotations <- c(
      step$content[[n]]$annotations,
      delta$annotations
    )
    return(step)
  }
  if (type == "thought_signature") {
    step$signature <- delta$signature
    return(step)
  }
  if (type == "thought_summary") {
    step$summary <- c(step$summary, list(delta$content))
    return(step)
  }
  if (type == "arguments_delta") {
    # Arrives as fragments of a JSON string; parsed in value_turn()
    previous <- if (is.character(step$arguments)) {
      step$arguments
    } else {
      ""
    }
    step$arguments <- paste0(previous, delta$arguments)
    return(step)
  }

  # Built-in tool deltas carry the remaining fields of the step. Merge
  # shallowly, since modifyList() would drop unnamed list elements
  fields <- delta[names(delta) != "type"]
  step[names(fields)] <- fields
  step
}

method(stream_content, ProviderGoogleInteractions) <- function(
  provider,
  event,
  completion = NULL
) {
  if (event$event_type == "step.delta") {
    delta <- event$delta
    if (identical(delta$type, "text")) {
      list(ContentText(delta$text))
    } else if (identical(delta$type, "thought_summary")) {
      list(ContentThinking(delta$content$text))
    } else {
      list()
    }
  } else if (
    event$event_type == "interaction.completed" && !is.null(completion)
  ) {
    # Citations and tool activity are rebuilt from the merged steps, since
    # the sources for a search only arrive with the answer text. So when
    # streaming they appear after the text; the saved turn is unaffected
    contents <- gemini_step_contents(completion$steps)
    keep(contents, function(content) {
      !is_stream_text_content(content) &&
        !S7_inherits(content, ContentToolRequest)
    })
  } else {
    list()
  }
}

# Batched requests -------------------------------------------------------------

method(has_batch_support, ProviderGoogleInteractions) <- function(provider) {
  TRUE
}

# The batch and countTokens endpoints still use generateContent, so these
# methods hand over to the sibling class, which builds and parses that format.
# This also means those requests use the generateContent error handling and
# retry rules, as they did before the switch to Interactions.
as_generate_content <- function(provider) {
  inject(ProviderGoogleGenerateContent(!!!provider_props(provider)))
}

method(batch_submit, ProviderGoogleInteractions) <- function(
  provider,
  model,
  conversations,
  type = NULL
) {
  batch_submit(as_generate_content(provider), model, conversations, type)
}

method(batch_result_turn, ProviderGoogleInteractions) <- function(
  provider,
  model,
  result,
  has_type = FALSE
) {
  batch_result_turn(as_generate_content(provider), model, result, has_type)
}

method(count_tokens, ProviderGoogleInteractions) <- function(
  provider,
  model,
  ...,
  system_prompt = NULL,
  tools = list(),
  type = NULL
) {
  count_tokens(
    as_generate_content(provider),
    model,
    ...,
    system_prompt = system_prompt,
    tools = tools,
    type = type
  )
}
