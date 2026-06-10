#' @include provider.R
#' @include content.R
#' @include turns.R
#' @include tools-def.R
#' @include tools-built-in.R
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
#' # Server-side MCP tools
#'
#' Claude's
#' [MCP connector](https://platform.claude.com/docs/en/docs/agents-and-tools/mcp-connector)
#' (beta) lets Claude connect to remote MCP servers directly. Unlike
#' local tool use, Claude discovers the tools available on the server
#' and handles tool execution on your behalf — no client-side MCP
#' handling is needed.
#'
#' Use [mcp_connector()] to register tools from an MCP server with a
#' chat:
#'
#' ```r
#' chat <- chat_anthropic()
#' connector <- mcp_connector(
#'   url = "https://mcp.deepwiki.com/mcp",
#'   name = "deepwiki"
#' )
#' chat$register_tool(connector)
#' chat$chat("Look up the tidyverse/ellmer repo with your deepwiki tools.")
#' ```
#'
#' If the MCP server requires authentication, pass a credential
#' function. The return value is sent as the `authorization_token`
#' field on the MCP server configuration.
#'
#' ```r
#' connector <- mcp_connector(
#'   url = "https://private-mcp-server.example.com/mcp",
#'   name = "private",
#'   credentials = function() Sys.getenv("MCP_SERVER_TOKEN")
#' )
#' chat$register_tool(connector)
#' ```
#'
#' @inheritParams chat_openai
#' @inherit chat_openai return
#' @param model `r param_model("claude-sonnet-4-5-20250929", "anthropic")`
#' @param api_key `r lifecycle::badge("deprecated")` Use `credentials` instead.
#' @param credentials `r api_key_param("ANTHROPIC_API_KEY")`
#' @param base_url The base URL to the endpoint; the default is Claude's
#'   public API.
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

# The id of the most recent code-execution container in the conversation, or
# NULL if none exists. Scanning newest-first reuses the freshest container so
# paused programmatic executions resume in place and follow-up turns can build
# on previously created sandbox files/state. Only AssistantTurn carries @json.
last_container_id <- function(turns, now = Sys.time()) {
  for (turn in rev(turns)) {
    if (!S7_inherits(turn, AssistantTurn)) {
      next
    }
    container <- turn@json$container
    if (is.null(container$id)) {
      next
    }
    # The newest container is the freshest; if it has already expired, older
    # ones have too, so reuse nothing and let the API create a new container.
    # Sending an expired id does not error but makes code execution silently
    # fail (the sandbox is gone).
    if (container_is_expired(container, now)) {
      return(NULL)
    }
    return(container$id)
  }
  NULL
}

# A container is expired when it carries an `expires_at` timestamp in the past.
# Without a (parseable) timestamp we assume it is still usable.
container_is_expired <- function(container, now = Sys.time()) {
  expires_at <- container$expires_at
  if (!is_string(expires_at)) {
    return(FALSE)
  }
  expiry <- as.POSIXct(expires_at, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
  !is.na(expiry) && expiry <= now
}

has_code_execution_tool <- function(tools) {
  some(tools, function(t) {
    S7_inherits(t, ToolBuiltIn) && identical(t@name, "code_execution")
  })
}

# Warn if any tool opts into programmatic calling (allowed_callers set) but no
# code_execution tool is registered, since Claude can then never invoke it from
# code. Deduped so a multi-turn tool loop does not repeat the warning.
warn_missing_code_execution_tool <- function(tools) {
  wants_programmatic <- some(tools, is_programmatic_tool)
  if (!wants_programmatic) {
    return(invisible())
  }

  if (has_code_execution_tool(tools)) {
    return(invisible())
  }

  cli::cli_warn(
    c(
      "A tool sets {.arg allowed_callers} but no code execution tool is registered.",
      i = "Register {.run claude_tool_code_execution()} to enable programmatic tool calling.",
      i = "Until then, the tool can only be called directly."
    ),
    .frequency = "regularly",
    .frequency_id = "ellmer_ptc_missing_code_tool"
  )
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
    if (
      has_claude_structured_output(provider@model) &&
        !type_has_additional_properties(type)
    ) {
      output_config <- list(
        format = list(
          type = "json_schema",
          schema = as_json(provider, type)
        )
      )
      tool_choice <- NULL
    } else {
      tool_def <- ToolDef(
        function(...) {},
        name = "_structured_tool_call",
        description = "Extract structured data",
        arguments = type_object(data = type)
      )
      tools[[tool_def@name]] <- tool_def
      tool_choice <- list(type = "tool", name = tool_def@name)
      stream <- FALSE
      output_config <- NULL
    }
  } else {
    tool_choice <- NULL
    output_config <- NULL
  }
  mcp_connectors <- keep(unname(tools), \(t) S7_inherits(t, McpConnector))
  if (length(mcp_connectors) > 0) {
    mcp_servers <- map(mcp_connectors, function(conn) {
      server <- compact(list(
        type = "url",
        url = conn@url,
        name = conn@name,
        authorization_token = if (!is.null(conn@credentials)) conn@credentials()
      ))
      modify_list(server, conn@extra)
    })
  } else {
    mcp_servers <- NULL
  }

  warn_missing_code_execution_tool(tools)
  # The API only accepts `container` when the code execution tool is enabled,
  # so requests that drop the tool (e.g. structured extraction, batches) must
  # not reuse the conversation's container.
  container <- if (has_code_execution_tool(tools)) last_container_id(turns)
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
    mcp_servers = mcp_servers,
    tool_choice = tool_choice,
    thinking = thinking,
    output_config = output_config,
    container = container,
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
method(stream_content, ProviderAnthropic) <- function(provider, event) {
  if (event$type == "content_block_delta") {
    if (identical(event$delta$type, "thinking_delta")) {
      return(ContentThinking(event$delta$thinking))
    }
    text <- event$delta$text
    if (is.null(text)) {
      return(NULL)
    }
    ContentText(text)
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
    } else if (chunk$delta$type == "citations_delta") {
      # https://docs.claude.com/en/docs/build-with-claude/citations#streaming-support
      result$content[[i]]$citations <- c(
        result$content[[i]]$citations,
        list(chunk$delta$citation)
      )
    } else {
      cli::cli_inform(c("!" = "Unknown delta type {.str {chunk$delta$type}}."))
    }
  } else if (chunk$type == "content_block_stop") {
    # nothing to do
  } else if (chunk$type == "message_delta") {
    result$stop_reason <- chunk$delta$stop_reason
    result$stop_sequence <- chunk$delta$stop_sequence
    result$usage$output_tokens <- chunk$usage$output_tokens
    # The code execution sandbox container id arrives nested under `delta` in
    # the streaming protocol (it's top-level in non-streaming responses). Lift
    # it to the top level so last_container_id() can reuse it across turns.
    if (!is.null(chunk$delta$container)) {
      result$container <- chunk$delta$container
    }
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
  usage <- json$usage
  tokens(
    input = (usage$input_tokens %||% 0) +
      (usage$cache_creation_input_tokens %||% 0),
    output = usage$output_tokens %||% 0,
    cached_input = usage$cache_read_input_tokens %||% 0
  )
}

method(value_turn, ProviderAnthropic) <- function(
  provider,
  result,
  has_type = FALSE
) {
  contents <- lapply(result$content, function(content) {
    if (content$type == "text") {
      if (has_type && has_claude_structured_output(provider@model)) {
        ContentJson(string = content$text)
      } else {
        ContentText(content$text)
      }
    } else if (content$type == "tool_use") {
      if (has_type) {
        ContentJson(data = content$input$data)
      } else {
        if (is_string(content$input)) {
          content$input <- jsonlite::parse_json(content$input)
        }
        ContentToolRequest(
          content$id,
          content$name,
          content$input,
          extra = compact(list(caller = content$caller))
        )
      }
    } else if (content$type == "server_tool_use") {
      if (is_string(content$input)) {
        content$input <- jsonlite::parse_json(content$input)
      }
      if (content$name == "web_search") {
        # https://docs.claude.com/en/docs/agents-and-tools/tool-use/web-search-tool#response
        ContentToolRequestSearch(
          query = content$input$query,
          json = content
        )
      } else if (content$name == "web_fetch") {
        # https://docs.claude.com/en/docs/agents-and-tools/tool-use/web-fetch-tool#response
        ContentToolRequestFetch(
          url = content$input$url,
          json = content
        )
      } else if (
        content$name %in%
          c(
            "code_execution",
            "bash_code_execution",
            "text_editor_code_execution"
          )
      ) {
        # https://docs.claude.com/en/docs/agents-and-tools/tool-use/code-execution-tool#response-format
        input <- content$input %||% list()
        ContentToolRequestCode(
          id = content$id,
          name = content$name,
          arguments = if (is.list(input)) input else list(input),
          json = content
        )
      } else {
        cli::cli_abort("Unknown server tool {.str {content$name}}.")
      }
    } else if (content$type == "web_search_tool_result") {
      urls <- map_chr(content$content, \(x) x$url)
      ContentToolResponseSearch(
        urls = urls,
        json = content
      )
    } else if (content$type == "web_fetch_tool_result") {
      ContentToolResponseFetch(url = content$url %||% "failed", json = content)
    } else if (
      content$type %in%
        c(
          "code_execution_tool_result",
          "bash_code_execution_tool_result",
          "text_editor_code_execution_tool_result"
        )
    ) {
      result_block <- content$content %||% list()
      is_error <- grepl("_error$", result_block$type %||% "")
      ContentToolResponseCode(
        value = if (!is_error) {
          code_execution_result_body(result_block)
        } else {
          NULL
        },
        error = if (is_error) result_block$error_code %||% "unknown" else NULL,
        request = ContentToolRequest(
          id = content$tool_use_id,
          name = "",
          arguments = list()
        ),
        json = content
      )
    } else if (content$type == "mcp_tool_use") {
      if (is_string(content$input)) {
        content$input <- jsonlite::parse_json(content$input)
      }
      input <- content$input %||% list()
      ContentMcpToolRequest(
        id = content$id,
        name = content$name,
        arguments = if (is.list(input)) input else list(input),
        tool = mcp_tool_def(content$name, content$server_name),
        server_name = content$server_name,
        json = content
      )
    } else if (content$type == "mcp_tool_result") {
      content_blocks <- content$content %||% list()
      text_blocks <- keep(content_blocks, \(b) identical(b$type, "text"))
      text <- paste(
        vapply(text_blocks, function(b) b$text %||% "", character(1)),
        collapse = "\n"
      )
      is_error <- content$is_error %||% FALSE
      result <- ContentMcpToolResult(
        value = if (!is_error) text else NULL,
        error = if (is_error) text else NULL,
        request = ContentToolRequest(
          id = content$tool_use_id,
          name = "",
          arguments = list()
        ),
        content = content_blocks,
        json = content
      )
      images <- lapply(
        keep(content_blocks, \(b) identical(b$type, "image")),
        \(b) {
          ContentImageInline(
            type = b$source$media_type %||% "image/png",
            data = b$source$data
          )
        }
      )
      c(list(result), images)
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
  # Flatten: mcp_tool_result may return a list of Content objects (result + images)
  contents <- unlist(contents, recursive = FALSE)

  # Link server-side tool results to their requests so @request carries the
  # tool name and arguments (used by display and shinychat tool cards).
  contents <- link_server_tool_results(
    contents,
    ContentMcpToolRequest,
    ContentMcpToolResult
  )
  contents <- link_server_tool_results(
    contents,
    ContentToolRequestCode,
    ContentToolResponseCode
  )

  tokens <- value_tokens(provider, result)
  cache_write <- result$usage$cache_creation_input_tokens %||% 0
  # Anthropic charges 1.25x the input rate for cache writes; tokens$input
  # already counts them at 1.0x, so add the 0.25x surcharge for pricing.
  cost_tokens <- tokens
  cost_tokens$input <- cost_tokens$input + cache_write * 0.25
  cost <- get_token_cost(provider, cost_tokens)
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
    x <- turn_contents_expand(x)
    # Serialize each block individually (rather than via the list method) so
    # we know which content object produced the last emitted block; blocks
    # that serialize to NULL are dropped, mirroring compact().
    json <- lapply(x@contents, function(content) {
      as_json(provider, content, ...)
    })
    emitted <- lengths(json) > 0
    content <- json[emitted]

    # Add caching to the last content block in the last turn
    # https://docs.claude.com/en/docs/build-with-claude/prompt-caching#how-automatic-prefix-checking-works
    # Exception: the API rejects cache_control on a tool result that was called
    # by code execution (programmatic tool calling) because those blocks aren't
    # rendered in Claude's context. Skip caching when the last block is one.
    if (is_last && any(emitted)) {
      last_content <- x@contents[emitted][[sum(emitted)]]
      if (!is_programmatic_tool_result(last_content)) {
        content[[length(content)]]$cache_control <- cache_control(provider)
      }
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

method(as_json, list(ProviderAnthropic, ContentUploaded)) <- function(
  provider,
  x
) {
  # https://docs.claude.com/en/docs/build-with-claude/files#using-a-file-in-messages
  block_type <- switch(
    x@mime_type,
    "application/pdf" = "document",
    "text/plain" = "document",
    "image/jpeg" = "image",
    "image/png" = "image",
    "image/gif" = "image",
    "image/webp" = "image",
    "text/csv" = "container_upload",
    "application/json" = "container_upload",
    "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" = "container_upload",
    "application/vnd.ms-excel" = "container_upload",
    "text/xml" = "container_upload",
    "application/xml" = "container_upload"
  )

  list(
    type = block_type,
    source = list(
      type = "file",
      file_id = x@uri
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
  # Build the list directly (do NOT wrap in compact()): compact() drops
  # zero-length elements, which would strip `input` for argument-free tools and
  # produce a request the API rejects. A bare empty list() also serializes to a
  # JSON array (`[]`); the API requires an object, so coerce it to an empty
  # named list (`{}`). `caller` is only present on programmatic tool calls
  # (captured into @extra in value_turn), so append it conditionally to keep
  # direct calls unchanged.
  input <- x@arguments
  if (length(input) == 0) {
    input <- set_names(list())
  }
  out <- list(
    type = "tool_use",
    id = x@id,
    name = x@name,
    input = input
  )
  if (!is.null(x@extra$caller)) {
    out$caller <- x@extra$caller
  }
  out
}

# https://docs.anthropic.com/en/docs/build-with-claude/tool-use#handling-tool-use-and-tool-result-content-blocks
method(as_json, list(ProviderAnthropic, ContentToolResult)) <- function(
  provider,
  x,
  ...
) {
  if (is.null(x@request)) {
    cli::cli_abort(
      "Can't serialize a tool result that has no associated tool request."
    )
  }
  list(
    type = "tool_result",
    tool_use_id = x@request@id,
    content = tool_string(x),
    is_error = tool_errored(x)
  )
}

method(as_json, list(ProviderAnthropic, ContentMcpToolRequest)) <- function(
  provider,
  x,
  ...
) {
  x@json
}

method(as_json, list(ProviderAnthropic, ContentMcpToolResult)) <- function(
  provider,
  x,
  ...
) {
  x@json
}

# Echo the raw server_tool_use / *_code_execution_tool_result blocks back
# verbatim. Without these provider-specific methods, S7 dispatches to the
# generic ContentToolRequest/ContentToolResult methods (which emit "tool_use"/
# "tool_result") and the API rejects the resent turn.
method(as_json, list(ProviderAnthropic, ContentToolRequestCode)) <- function(
  provider,
  x,
  ...
) {
  x@json
}

method(as_json, list(ProviderAnthropic, ContentToolResponseCode)) <- function(
  provider,
  x,
  ...
) {
  x@json
}

method(as_json, list(ProviderAnthropic, ToolDef)) <- function(
  provider,
  x,
  ...
) {
  compact(list(
    name = x@name,
    description = x@description,
    input_schema = compact(as_json(provider, x@arguments, ...)),
    allowed_callers = if (length(x@allowed_callers)) {
      as.list(x@allowed_callers)
    }
  ))
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

# MCP connector support --------------------------------------------------------

method(check_mcp_connector_tool, ProviderAnthropic) <- function(
  provider,
  tool,
  ...,
  error_call = caller_env()
) {
  invisible()
}

# Anthropic supports programmatic tool calling, so allowed_callers is honoured.
method(check_programmatic_tool, ProviderAnthropic) <- function(provider, tool) {
  invisible()
}

method(as_json, list(ProviderAnthropic, McpConnector)) <- function(
  provider,
  x,
  ...
) {
  list(type = "mcp_toolset", mcp_server_name = x@name)
}

method(chat_request, ProviderAnthropic) <- function(
  provider,
  stream = TRUE,
  turns = list(),
  tools = list(),
  type = NULL
) {
  has_mcp <- any(map_lgl(tools, \(t) S7_inherits(t, McpConnector)))

  req <- base_request(provider)
  req <- req_url_path_append(req, chat_path(provider))

  body <- chat_body(
    provider = provider,
    stream = stream,
    turns = turns,
    tools = tools,
    type = type
  )
  extra_tools <- provider@extra_args[["tools"]]
  extra_args <- provider@extra_args
  extra_args[["tools"]] <- NULL
  body <- modify_list(body, extra_args)
  if (!is.null(extra_tools)) {
    body[["tools"]] <- c(body[["tools"]], extra_tools)
  }
  req <- req_body_json(req, body)
  req <- req_headers(req, !!!provider@extra_headers)

  if (has_mcp) {
    beta <- unique(c(provider@beta_headers, "mcp-client-2025-11-20"))
    req <- req_headers(req, `anthropic-beta` = paste(beta, collapse = ","))
  }

  req
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
models_claude <- function(
  base_url = "https://api.anthropic.com/v1",
  api_key = NULL,
  credentials = NULL
) {
  credentials <- as_credentials(
    "models_anthropic",
    function() anthropic_key(),
    credentials = credentials,
    api_key = api_key
  )

  provider <- ProviderAnthropic(
    name = "Anthropic",
    model = "",
    base_url = base_url,
    credentials = credentials,
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

#' @export
#' @rdname chat_anthropic
models_anthropic <- models_claude

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

has_claude_structured_output <- function(model) {
  # Matches Claude 4.5+ models
  # https://platform.claude.com/docs/en/build-with-claude/structured-outputs
  grepl("^claude-\\w+-4-[5-9](-|$)", model)
}
