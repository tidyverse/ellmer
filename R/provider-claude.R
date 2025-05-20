#' @include provider.R
#' @include content.R
#' @include turns.R
#' @include tools-def.R
NULL

#' Chat with an Anthropic Claude model
#'
#' @description
#' [Anthropic](https://www.anthropic.com) provides a number of chat based
#' models under the [Claude](https://www.anthropic.com/claude) moniker.
#' Note that a Claude Pro membership does not give you the ability to call
#' models via the API; instead, you will need to sign up (and pay for) a
#' [developer account](https://console.anthropic.com/).
#'
#' @inheritParams chat_openai
#' @inherit chat_openai return
#' @param model `r param_model("claude-3-7-sonnet-latest", "anthropic")`
#' @param api_key `r api_key_param("ANTHROPIC_API_KEY")`
#' @param max_tokens Maximum number of tokens to generate before stopping.
#' @param beta_headers Optionally, a character vector of beta headers to opt-in
#'   claude features that are still in beta.
#' @family chatbots
#' @export
#' @examplesIf has_credentials("claude")
#' chat <- chat_anthropic()
#' chat$chat("Tell me three jokes about statisticians")
chat_anthropic <- function(
  system_prompt = NULL,
  params = NULL,
  max_tokens = deprecated(),
  model = NULL,
  api_args = list(),
  base_url = "https://api.anthropic.com/v1",
  beta_headers = character(),
  api_key = anthropic_key(),
  echo = NULL
) {
  echo <- check_echo(echo)

  model <- set_default(model, "claude-3-7-sonnet-latest")

  params <- params %||% params()
  if (lifecycle::is_present(max_tokens)) {
    lifecycle::deprecate_warn(
      when = "0.2.0",
      what = "chat_anthropic(max_tokens)",
      with = "chat_anthropic(params)"
    )
    params$max_tokens <- max_tokens
  }

  provider <- ProviderAnthropic(
    name = "Anthropic",
    model = model,
    params = params %||% params(),
    extra_args = api_args,
    base_url = base_url,
    beta_headers = beta_headers,
    api_key = api_key
  )

  Chat$new(provider = provider, system_prompt = system_prompt, echo = echo)
}

chat_anthropic_test <- function(
  ...,
  model = "claude-3-5-sonnet-latest",
  params = NULL
) {
  params <- params %||% params()
  if (is_testing()) {
    params$temperature <- params$temperature %||% 0
  }

  chat_anthropic(model = model, params = params, ...)
}

ProviderAnthropic <- new_class(
  "ProviderAnthropic",
  parent = Provider,
  properties = list(
    api_key = prop_string(),
    beta_headers = class_character
  )
)

anthropic_key <- function() {
  key_get("ANTHROPIC_API_KEY")
}
anthropic_key_exists <- function() {
  key_exists("ANTHROPIC_API_KEY")
}

method(base_request, ProviderAnthropic) <- function(provider) {
  req <- request(provider@base_url)
  # <https://docs.anthropic.com/en/api/versioning>
  req <- req_headers(req, `anthropic-version` = "2023-06-01")
  # <https://docs.anthropic.com/en/api/getting-started#authentication>
  req <- req_headers_redacted(req, `x-api-key` = provider@api_key)
  # <https://docs.anthropic.com/en/api/rate-limits>
  req <- req_retry(
    req,
    # <https://docs.anthropic.com/en/api/errors#http-errors>
    is_transient = function(resp) resp_status(resp) %in% c(429, 503, 529),
    max_tries = 2
  )
  req <- ellmer_req_timeout(req, stream)

  if (length(provider@beta_headers) > 0) {
    req <- req_headers(req, `anthropic-beta` = provider@beta_headers)
  }

  # <https://docs.anthropic.com/en/api/errors>
  req <- req_error(req, body = function(resp) {
    if (resp_content_type(resp) == "application/json") {
      json <- resp_body_json(resp)
      paste0(json$error$message, " [", json$error$type, "]")
    }
  })

  req
}


# https://docs.anthropic.com/en/api/messages
method(chat_path, ProviderAnthropic) <- function(provider) {
  "messages"
}
method(chat_body, ProviderAnthropic) <- function(
  provider,
  stream = TRUE,
  turns = list(),
  tools = list(),
  type = NULL
) {
  if (length(turns) >= 1 && is_system_prompt(turns[[1]])) {
    system <- turns[[1]]@text
  } else {
    system <- NULL
  }

  messages <- compact(as_json(provider, turns))

  if (!is.null(type)) {
    tool_def <- ToolDef(
      fun = function(...) {
      },
      name = "_structured_tool_call",
      description = "Extract structured data",
      arguments = type_object(data = type)
    )
    tools[[tool_def@name]] <- tool_def
    tool_choice <- list(type = "tool", name = tool_def@name)
    stream <- FALSE
  } else {
    tool_choice <- NULL
  }
  tools <- as_json(provider, unname(tools))

  params <- chat_params(provider, provider@params)

  compact(list2(
    model = provider@model,
    system = system,
    messages = messages,
    stream = stream,
    tools = tools,
    tool_choice = tool_choice,
    !!!params
  ))
}

method(chat_params, ProviderAnthropic) <- function(provider, params) {
  params <- standardise_params(
    params,
    c(
      temperature = "temperature",
      top_p = "top_p",
      top_k = "top_k",
      max_tokens = "max_tokens",
      stop_sequences = "stop_sequences"
    )
  )

  # Unlike other providers, Claude requires that this be set
  params$max_tokens <- params$max_tokens %||% 4096

  params$stop_sequences <- as.list(params$stop_sequences)

  params
}

# Claude -> ellmer --------------------------------------------------------------

method(stream_parse, ProviderAnthropic) <- function(provider, event) {
  if (is.null(event)) {
    cli::cli_abort("Connection closed unexpectedly")
  }

  data <- jsonlite::parse_json(event$data)
  if (identical(data$type, "message_stop")) {
    return(NULL)
  }

  data
}
method(stream_text, ProviderAnthropic) <- function(provider, event) {
  if (event$type == "content_block_delta") {
    event$delta$text
  }
}
method(stream_merge_chunks, ProviderAnthropic) <- function(
  provider,
  result,
  chunk
) {
  if (chunk$type == "ping") {
    # nothing to do
  } else if (chunk$type == "message_start") {
    result <- chunk$message
  } else if (chunk$type == "content_block_start") {
    result$content[[chunk$index + 1L]] <- chunk$content_block
  } else if (chunk$type == "content_block_delta") {
    # https://docs.anthropic.com/en/api/messages-streaming#delta-types
    i <- chunk$index + 1L

    if (chunk$delta$type == "text_delta") {
      paste(result$content[[i]]$text) <- chunk$delta$text
    } else if (chunk$delta$type == "input_json_delta") {
      if (chunk$delta$partial_json != "") {
        # See issue #228 about partial_json sometimes being ""
        paste(result$content[[i]]$input) <- chunk$delta$partial_json
      }
    } else if (chunk$delta$type == "thinking_delta") {
      paste(result$content[[i]]$thinking) <- chunk$delta$thinking
    } else if (chunk$delta$type == "signature_delta") {
      paste(result$content[[i]]$signature) <- chunk$delta$signature
    } else {
      cli::cli_inform(c("!" = "Unknown delta type {.str {chunk$delta$type}}."))
    }
  } else if (chunk$type == "content_block_stop") {
    # nothing to do
  } else if (chunk$type == "message_delta") {
    result$stop_reason <- chunk$delta$stop_reason
    result$stop_sequence <- chunk$delta$stop_sequence
    result$usage$output_tokens <- chunk$usage$output_tokens
  } else if (chunk$type == "error") {
    if (chunk$error$type == "overloaded_error") {
      # https://docs.anthropic.com/en/api/messages-streaming#error-events
      # TODO: track number of retries
      wait <- backoff_default(1)
      Sys.sleep(wait)
    } else {
      cli::cli_abort("{chunk$error$message}")
    }
  } else {
    cli::cli_inform(c("!" = "Unknown chunk type {.str {chunk$type}}."))
  }
  result
}

method(value_turn, ProviderAnthropic) <- function(
  provider,
  result,
  has_type = FALSE
) {
  contents <- lapply(result$content, function(content) {
    if (content$type == "text") {
      ContentText(content$text)
    } else if (content$type == "tool_use") {
      if (has_type) {
        ContentJson(content$input$data)
      } else {
        if (is_string(content$input)) {
          content$input <- jsonlite::parse_json(content$input)
        }
        ContentToolRequest(content$id, content$name, content$input)
      }
    } else if (content$type == "thinking") {
      ContentThinking(
        content$thinking,
        extra = list(signature = content$signature)
      )
    } else {
      cli::cli_abort(
        "Unknown content type {.str {content$type}}.",
        .internal = TRUE
      )
    }
  })

  tokens <- tokens_log(
    provider,
    input = result$usage$input_tokens,
    output = result$usage$output_tokens
  )

  assistant_turn(contents, json = result, tokens = tokens)
}

# ellmer -> Claude --------------------------------------------------------------

method(as_json, list(ProviderAnthropic, Turn)) <- function(provider, x) {
  if (x@role == "system") {
    # claude passes system prompt as separate arg
    NULL
  } else if (x@role %in% c("user", "assistant")) {
    list(role = x@role, content = as_json(provider, x@contents))
  } else {
    cli::cli_abort("Unknown role {turn@role}", .internal = TRUE)
  }
}

method(as_json, list(ProviderAnthropic, ContentText)) <- function(provider, x) {
  if (is_whitespace(x@text)) {
    list(type = "text", text = "[empty string]")
  } else {
    list(type = "text", text = x@text)
  }
}

method(as_json, list(ProviderAnthropic, ContentPDF)) <- function(provider, x) {
  list(
    type = "document",
    source = list(
      type = "base64",
      media_type = x@type,
      data = x@data
    )
  )
}

method(as_json, list(ProviderAnthropic, ContentImageRemote)) <- function(
  provider,
  x
) {
  list(
    type = "image",
    source = list(
      type = "url",
      url = x@url
    )
  )
}

method(as_json, list(ProviderAnthropic, ContentImageInline)) <- function(
  provider,
  x
) {
  list(
    type = "image",
    source = list(
      type = "base64",
      media_type = x@type,
      data = x@data
    )
  )
}

# https://docs.anthropic.com/en/docs/build-with-claude/tool-use#handling-tool-use-and-tool-result-content-blocks
method(as_json, list(ProviderAnthropic, ContentToolRequest)) <- function(
  provider,
  x
) {
  list(
    type = "tool_use",
    id = x@id,
    name = x@name,
    input = x@arguments
  )
}

# https://docs.anthropic.com/en/docs/build-with-claude/tool-use#handling-tool-use-and-tool-result-content-blocks
method(as_json, list(ProviderAnthropic, ContentToolResult)) <- function(
  provider,
  x
) {
  list(
    type = "tool_result",
    tool_use_id = x@request@id,
    content = tool_string(x),
    is_error = tool_errored(x)
  )
}

method(as_json, list(ProviderAnthropic, ToolDef)) <- function(provider, x) {
  list(
    name = x@name,
    description = x@description,
    input_schema = compact(as_json(provider, x@arguments))
  )
}

method(as_json, list(ProviderAnthropic, ContentThinking)) <- function(
  provider,
  x
) {
  if (identical(x@thinking, "")) {
    return()
  }

  list(
    type = "thinking",
    thinking = x@thinking,
    signature = x@extra$signature
  )
}

# Batch chat -------------------------------------------------------------------

method(has_batch_support, ProviderAnthropic) <- function(provider) {
  TRUE
}

# https://docs.anthropic.com/en/api/creating-message-batches
method(batch_submit, ProviderAnthropic) <- function(
  provider,
  conversations,
  type = NULL
) {
  req <- base_request(provider)
  req <- req_url_path_append(req, "/messages/batches")

  requests <- map(seq_along(conversations), function(i) {
    params <- chat_body(
      provider,
      stream = FALSE,
      turns = conversations[[i]],
      type = type
    )
    list(
      custom_id = paste0("chat-", i),
      params = params
    )
  })
  req <- req_body_json(req, list(requests = requests))

  resp <- req_perform(req)
  resp_body_json(resp)
}

# https://docs.anthropic.com/en/api/retrieving-message-batches
method(batch_poll, ProviderAnthropic) <- function(provider, batch) {
  req <- base_request(provider)
  req <- req_url_path_append(req, "/messages/batches/", batch$id)
  resp <- req_perform(req)

  resp_body_json(resp)
}

method(batch_status, ProviderAnthropic) <- function(provider, batch) {
  counts <- batch$request_counts
  list(
    working = batch$processing_status != "ended",
    n_processing = batch$request_counts$processing,
    n_succeeded = batch$request_counts$succeeded,
    n_failed = counts$errored + counts$canceled + counts$expired
  )
}

# https://docs.anthropic.com/en/api/retrieving-message-batch-results
method(batch_retrieve, ProviderAnthropic) <- function(provider, batch) {
  req <- base_request(provider)
  req <- req_url(req, batch$results_url)
  req <- req_progress(req, "down")

  path <- withr::local_tempfile()
  req <- req_perform(req, path = path)

  lines <- readLines(path, warn = FALSE)
  json <- lapply(lines, jsonlite::fromJSON, simplifyVector = FALSE)

  ids <- as.numeric(gsub("chat-", "", map_chr(json, "[[", "custom_id")))
  results <- lapply(json, "[[", "result")
  results[order(ids)]
}

method(batch_result_turn, ProviderAnthropic) <- function(
  provider,
  result,
  has_type = FALSE
) {
  if (result$type == "succeeded") {
    value_turn(provider, result$message, has_type = has_type)
  } else {
    NULL
  }
}

# Pricing ----------------------------------------------------------------------

method(standardise_model, ProviderAnthropic) <- function(provider, model) {
  gsub("-(latest|\\d{8})$", "", model)
}

# Models -----------------------------------------------------------------------

#' @export
#' @rdname chat_anthropic
models_anthropic <- function(
  base_url = "https://api.anthropic.com/v1",
  api_key = anthropic_key()
) {
  provider <- ProviderAnthropic(
    name = "Anthropic",
    model = "",
    base_url = base_url,
    api_key = api_key
  )

  req <- base_request(provider)
  req <- req_url_path_append(req, "/models")
  resp <- req_perform(req)

  json <- resp_body_json(resp)

  id <- map_chr(json$data, "[[", "id")
  display_name <- map_chr(json$data, "[[", "display_name")
  created_at <- as.POSIXct(map_chr(json$data, "[[", "created_at"))

  df <- data.frame(
    id = id,
    name = display_name,
    created_at = created_at
  )
  model_name <- standardise_model(provider, df$id)
  df <- cbind(df, match_prices("Anthropic", model_name))
  df[order(-xtfrm(df$created_at)), ]
}

# Helpers ----------------------------------------------------------------

# From httr2
backoff_default <- function(i) {
  round(min(stats::runif(1, min = 1, max = 2^i), 60), 1)
}
