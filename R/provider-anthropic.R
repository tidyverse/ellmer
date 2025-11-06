#' @include provider.R
#' @include content.R
#' @include turns.R
#' @include tools-def.R
NULL

#' Chat with an Anthropic Claude model
#'
#' @description
#' [Anthropic](https://www.anthropic.com) provides a number of chat based models
#' under the [Claude](https://claude.com/product/overview) moniker. Note that a
#' Claude Pro membership does not give you the ability to call models via the
#' API; instead, you will need to sign up (and pay for) a
#' [developer account](https://platform.claude.com/).
#'
#' # Caching
#'
#' Caching with Claude is a bit more complicated than other providers but we
#' believe that on average it will save you both money and time, so we have
#' enabled it by default. With other providers, like OpenAI and Google,
#' you only pay for cache reads, which cost 10% of the normal price. With
#' Claude, you also pay for cache writes, which cost 125% of the normal price
#' for 5 minute caching and 200% of the normal price for 1 hour caching.
#'
#' How does this affect the total cost of a conversation? Imagine the first
#' turn sends 1000 input tokens and receives 200 output tokens. The second
#' turn must first send both the input and output from the previous turn
#' (1200 tokens). It then sends a further 1000 tokens and receives 200 tokens
#' back.
#'
#' To compare the prices of these two approaches we can ignore the cost of
#' output tokens, because they are the same for both. How much will the input
#' tokens cost? If we don't use caching, we send 1000 tokens in the first turn
#' and 2200 (1000 + 200 + 1000) tokens in the second turn for a total of 3200
#' tokens. If we use caching, we'll send (the equivalent of) 1000 * 1.25 = 1250
#' tokens in the first turn. In the second turn, 1000 of the input tokens will
#' be cached so the total cost is 1000 * 0.1 + (200 + 1000) * 1.25 = 1600
#' tokens. That makes a total of 2850 tokens, i.e. 11% fewer tokens,
#' decreasing the overall cost.
#'
#' Obviously, the details will vary from conversation to conversation, but
#' if you have a large system prompt that you re-use many times you should
#' expect to see larger savings. You can see exactly how many input and
#' cache input tokens each turn uses, along with the total cost,
#' with `chat$get_tokens()`. If you don't see savings for your use case, you can
#' suppress caching with `cache = "none"`.
#'
#' I know this is already quite complicated, but there's one final wrinkle:
#' Claude will only cache longer prompts, with caching requiring at least
#' 1024-4096 tokens, depending on the model. So don't be surprised it if you
#' don't see any differences with caching if you have a short prompt.
#'
#' See all the details at
#' <https://docs.claude.com/en/docs/build-with-claude/prompt-caching>.
#'
#' @inheritParams chat_openai
#' @inherit chat_openai return
#' @param model `r param_model("claude-sonnet-4-5-20250929", "anthropic")`
#' @param api_key `r lifecycle::badge("deprecated")` Use `credentials` instead.
#' @param credentials `r api_key_param("ANTHROPIC_API_KEY")`
#' @param cache How long to cache inputs? Defaults to "5m" (five minutes).
#'   Set to "none" to disable caching or "1h" to cache for one hour.
#'
#'   See details below.
#' @param beta_headers Optionally, a character vector of beta headers to opt-in
#'   claude features that are still in beta.
#' @param api_headers Named character vector of arbitrary extra headers appended
#'   to every chat API call.
#' @family chatbots
#' @export
#' @examples
#' \dontshow{ellmer:::vcr_example_start("chat_anthropic")}
#' chat <- chat_anthropic()
#' chat$chat("Tell me three jokes about statisticians")
#' \dontshow{ellmer:::vcr_example_end()}
chat_anthropic <- function(
  system_prompt = NULL,
  params = NULL,
  model = NULL,
  cache = c("5m", "1h", "none"),
  api_args = list(),
  base_url = "https://api.anthropic.com/v1",
  beta_headers = character(),
  api_key = NULL,
  credentials = NULL,
  api_headers = character(),
  echo = NULL
) {
  echo <- check_echo(echo)

  model <- set_default(model, "claude-sonnet-4-5-20250929")
  cache <- arg_match(cache)

  credentials <- as_credentials(
    "chat_anthropic",
    function() anthropic_key(),
    credentials = credentials,
    api_key = api_key
  )

  provider <- ProviderAnthropic(
    name = "Anthropic",
    model = model,
    params = params %||% params(),
    extra_args = api_args,
    extra_headers = api_headers,
    base_url = base_url,
    beta_headers = beta_headers,
    credentials = credentials,
    cache = cache
  )

  Chat$new(provider = provider, system_prompt = system_prompt, echo = echo)
}

#' @rdname chat_anthropic
#' @export
chat_claude <- chat_anthropic

chat_anthropic_test <- function(
  ...,
  model = "claude-sonnet-4-5-20250929",
  params = NULL,
  echo = "none"
) {
  params <- params %||% params()
  params$temperature <- params$temperature %||% 0

  chat_anthropic(model = model, params = params, ..., echo = echo)
}

ProviderAnthropic <- new_class(
  "ProviderAnthropic",
  parent = Provider,
  properties = list(
    beta_headers = class_character,
    cache = prop_string()
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
  req <- ellmer_req_credentials(req, provider@credentials(), "x-api-key")

  # <https://docs.anthropic.com/en/api/rate-limits>
  # <https://docs.anthropic.com/en/api/errors#http-errors>
  req <- ellmer_req_robustify(req, is_transient = function(resp) {
    resp_status(resp) %in% c(429, 503, 529)
  })

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
  if (length(turns) >= 1 && is_system_turn(turns[[1]])) {
    system <- list(list(type = "text", text = turns[[1]]@text))
    # Always cache system prompt
    system[[1]]$cache_control <- cache_control(provider)
  } else {
    system <- NULL
  }

  is_last <- seq_along(turns) == length(turns)
  messages <- compact(map2(turns, is_last, function(turn, is_last) {
    as_json(provider, turn, is_last = is_last)
  }))

  if (!is.null(type)) {
    tool_def <- ToolDef(
      function(...) {},
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
  if (has_name(params, "budget_tokens")) {
    thinking <- list(
      type = "enabled",
      budget_tokens = params$budget_tokens
    )
    params$budget_tokens <- NULL
  } else {
    thinking <- NULL
  }

  compact(list2(
    model = provider@model,
    system = system,
    messages = messages,
    stream = stream,
    tools = tools,
    tool_choice = tool_choice,
    thinking = thinking,
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
      stop_sequences = "stop_sequences",
      budget_tokens = "reasoning_tokens"
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
    event$delta$text %||% event$delta$thinking
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

method(value_tokens, ProviderAnthropic) <- function(provider, json) {
  tokens(
    # Hack in pricing for cache writes
    input = json$usage$input_tokens +
      json$usage$cache_creation_input_tokens * 1.25,
    output = json$usage$output_tokens,
    cached_input = json$usage$cache_read_input_tokens
  )
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
        ContentJson(data = content$input$data)
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

  tokens <- value_tokens(provider, result)
  cost <- get_token_cost(provider, tokens)
  AssistantTurn(contents, json = result, tokens = unlist(tokens), cost = cost)
}

# ellmer -> Claude --------------------------------------------------------------

method(as_json, list(ProviderAnthropic, Turn)) <- function(
  provider,
  x,
  ...,
  is_last = FALSE
) {
  if (is_system_turn(x)) {
    # claude passes system prompt as separate arg
    NULL
  } else if (is_user_turn(x) || is_assistant_turn(x)) {
    if (is_assistant_turn(x) && identical(x@contents, list())) {
      # Drop empty assistant turns to avoid an API error
      # (all messages must have non-empty content)
      return(NULL)
    }

    # Add caching to the last content block in the last turn
    # https://docs.claude.com/en/docs/build-with-claude/prompt-caching#how-automatic-prefix-checking-works
    content <- as_json(provider, x@contents, ...)
    if (is_last) {
      content[[length(content)]]$cache_control <- cache_control(provider)
    }
    list(role = x@role, content = content)
  } else {
    cli::cli_abort("Unknown role {x@role}", .internal = TRUE)
  }
}

method(as_json, list(ProviderAnthropic, ContentText)) <- function(
  provider,
  x,
  ...
) {
  if (is_whitespace(x@text)) {
    list(type = "text", text = "[empty string]")
  } else {
    list(type = "text", text = x@text)
  }
}

method(as_json, list(ProviderAnthropic, ContentPDF)) <- function(
  provider,
  x,
  ...
) {
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
  x,
  ...
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
  x,
  ...
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
  x,
  ...
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
  x,
  ...
) {
  list(
    type = "tool_result",
    tool_use_id = x@request@id,
    content = tool_string(x),
    is_error = tool_errored(x)
  )
}

method(as_json, list(ProviderAnthropic, ToolDef)) <- function(
  provider,
  x,
  ...
) {
  list(
    name = x@name,
    description = x@description,
    input_schema = compact(as_json(provider, x@arguments, ...))
  )
}

method(as_json, list(ProviderAnthropic, ContentThinking)) <- function(
  provider,
  x,
  ...
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
  req <- req_url_path_append(req, "/messages/batches", batch$id)
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
    credentials = function() api_key,
    cache = "none"
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
  df <- cbind(df, match_prices("Anthropic", df$id))
  df[order(-xtfrm(df$created_at)), ]
}

# Helpers ----------------------------------------------------------------

# From httr2
backoff_default <- function(i) {
  round(min(stats::runif(1, min = 1, max = 2^i), 60), 1)
}

cache_control <- function(provider) {
  if (provider@cache == "none") {
    NULL
  } else {
    list(
      type = "ephemeral",
      ttl = provider@cache
    )
  }
}
