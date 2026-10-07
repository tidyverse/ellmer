otel_tracer_name <- "co.posit.r-package.ellmer"

otel_cache_tracer <- NULL
otel_capture_content_enabled <- NULL
local_chat_otel_span <- NULL
local_tool_otel_span <- NULL
local_agent_otel_span <- NULL
otel_record_histogram <- NULL

# Histograms from the GenAI semantic conventions for metrics. The
# `gen_ai.client.inference.*` instruments are defined by the client inference
# conventions; the agent and tool instruments by the general metrics page.
# See: https://github.com/open-telemetry/semantic-conventions-genai/blob/main/docs/gen-ai/client-inference.md
# See: https://opentelemetry.io/docs/specs/semconv/gen-ai/gen-ai-metrics/
#
# The conventions also advise explicit bucket boundaries (e.g. 0.01 to 81.92 s
# for durations, 1 to 67,108,864 for token counts), but
# `otel::meter$create_histogram()` only accepts a name, description, and unit,
# so the SDK's default buckets are used (see r-lib/otel#49). Users can
# configure views in their collector or SDK to apply the advised boundaries.
otel_histogram_specs <- list(
  "gen_ai.client.inference.duration" = list(
    description = "GenAI client inference operation duration",
    unit = "s"
  ),
  "gen_ai.client.inference.operation.input_tokens" = list(
    description = "Number of input tokens used per inference operation",
    unit = "{token}"
  ),
  "gen_ai.client.inference.operation.output_tokens" = list(
    description = "Number of output tokens used per inference operation",
    unit = "{token}"
  ),
  "gen_ai.client.inference.time_to_first_chunk" = list(
    description = "Time to receive the first chunk of a streamed response",
    unit = "s"
  ),
  "gen_ai.execute_tool.duration" = list(
    description = "The duration of a single tool execution",
    unit = "s"
  ),
  "gen_ai.invoke_agent.duration" = list(
    description = "The end-to-end duration of a single agent invocation",
    unit = "s"
  ),
  "gen_ai.invoke_agent.inference_calls" = list(
    description = "The number of inference calls made during an agent invocation",
    unit = "{inference_call}"
  ),
  "gen_ai.invoke_agent.tool_calls" = list(
    description = "The number of tool calls made during an agent invocation",
    unit = "{tool_call}"
  )
)

# `gen_ai.provider.name` well-known values, keyed by the provider's display
# name. Other providers fall back to the lowercased display name.
# See: https://opentelemetry.io/docs/specs/semconv/registry/attributes/gen-ai/
otel_provider_names <- c(
  "Anthropic" = "anthropic",
  "AWS/Bedrock" = "aws.bedrock",
  "Azure/OpenAI" = "azure.ai.openai",
  "DeepSeek" = "deepseek",
  "Google/Gemini" = "gcp.gemini",
  "Google/Vertex" = "gcp.vertex_ai",
  "Groq" = "groq",
  "Mistral" = "mistral_ai",
  "OpenAI" = "openai",
  "Perplexity" = "perplexity"
)

otel_provider_name <- function(provider) {
  name <- provider@name
  if (name %in% names(otel_provider_names)) {
    otel_provider_names[[name]]
  } else {
    tolower(name)
  }
}

# Map `params()` onto the `gen_ai.request.*` span attributes.
as_otel_int <- function(x) {
  if (!is.null(x)) as.integer(x)
}

otel_request_attributes <- function(model) {
  p <- model@params
  compact(list(
    "gen_ai.request.temperature" = p$temperature,
    "gen_ai.request.top_p" = p$top_p,
    # semconv types these as `int`; `params()` stores them as doubles.
    "gen_ai.request.top_k" = as_otel_int(p$top_k),
    "gen_ai.request.frequency_penalty" = p$frequency_penalty,
    "gen_ai.request.presence_penalty" = p$presence_penalty,
    "gen_ai.request.seed" = as_otel_int(p$seed),
    "gen_ai.request.max_tokens" = as_otel_int(p$max_tokens),
    "gen_ai.request.stop_sequences" = p$stop_sequences,
    "gen_ai.request.reasoning.level" = p$reasoning_effort
  ))
}

# `server.address` and `server.port` from the provider's base URL. The port
# falls back to the scheme default since semconv requires it when the address
# is set.
otel_server_attributes <- function(provider) {
  url <- tryCatch(httr2::url_parse(provider@base_url), error = function(e) NULL)
  if (is.null(url) || is.null(url$hostname)) {
    return(list())
  }
  port <- url$port %||% switch(url$scheme %||% "", https = 443, http = 80)
  compact(list(
    # IPv6 literals are bracketed in URLs but not in `server.address`.
    "server.address" = sub("^\\[(.*)\\]$", "\\1", url$hostname),
    "server.port" = if (!is.null(port)) as.integer(port)
  ))
}

# A GenAI semconv tool definition for a ToolDef. Built-in (provider-executed)
# tools have no argument schema and are omitted by the caller.
as_otel_tool_definition <- function(tool, provider) {
  parameters <- as_json(provider, tool@arguments)
  compact(list(
    type = "function",
    name = tool@name,
    description = tool@description,
    # Some providers serialize an empty schema as `[]`, which is not a valid
    # JSON Schema object, so omit it instead.
    parameters = if (length(parameters)) parameters
  ))
}

local({
  otel_is_tracing <- FALSE
  otel_tracer <- NULL
  otel_capture_content <- FALSE
  otel_is_measuring <- FALSE
  otel_histograms <- list()

  otel_cache_tracer <<- function() {
    if (!requireNamespace("otel", quietly = TRUE)) {
      return()
    }
    otel_tracer <<- otel::get_tracer(otel_tracer_name)
    otel_is_tracing <<- tracer_enabled(otel_tracer)
    otel_capture_content <<- {
      val <- Sys.getenv("OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT")
      tolower(val) %in% c("true", "1")
    }

    otel_meter <- otel::get_meter(otel_tracer_name)
    otel_is_measuring <<- otel::is_measuring_enabled(otel_meter)
    otel_histograms <<- if (otel_is_measuring) {
      imap(otel_histogram_specs, function(spec, name) {
        otel_meter$create_histogram(name, spec$description, unit = spec$unit)
      })
    }
  }

  otel_capture_content_enabled <<- function() otel_capture_content

  otel_record_histogram <<- function(name, value, attributes) {
    if (!otel_is_measuring) {
      return()
    }
    otel_histograms[[name]]$record(value, attributes = compact(attributes))
    invisible()
  }

  local_chat_otel_span <<- function(
    provider,
    model,
    turns = NULL,
    system_prompt = NULL,
    parent = NULL,
    conversation_id = NULL,
    stream = FALSE,
    type = NULL,
    tools = NULL,
    local_envir = parent.frame()
  ) {
    if (!otel_is_tracing) {
      return()
    }
    chat_span <-
      otel::start_span(
        sprintf("chat %s", model@name),
        options = list(
          parent = parent,
          kind = "client"
        ),
        # Per the GenAI semantic conventions, gen_ai.conversation.id is only
        # set when the caller has a conversation identifier readily available;
        # never invent a fallback value.
        attributes = c(
          compact(list(
            "gen_ai.operation.name" = "chat",
            "gen_ai.provider.name" = otel_provider_name(provider),
            "gen_ai.request.model" = model@name,
            "gen_ai.conversation.id" = conversation_id,
            # Only set when streaming; unset means non-streaming per semconv.
            "gen_ai.request.stream" = if (stream) TRUE,
            "gen_ai.output.type" = if (!is.null(type)) "json"
          )),
          otel_request_attributes(model),
          otel_server_attributes(provider)
        ),
        tracer = otel_tracer
      )

    defer(otel::end_span(chat_span), envir = local_envir)

    if (otel_capture_content) {
      tools <- Filter(\(tool) S7_inherits(tool, ToolDef), unname(tools))
      if (length(tools)) {
        defs <- lapply(tools, as_otel_tool_definition, provider = provider)
        chat_span$set_attribute(
          "gen_ai.tool.definitions",
          jsonlite::toJSON(defs, auto_unbox = TRUE, null = "null")
        )
      }
      if (!is.null(system_prompt)) {
        parts <- lapply(system_prompt@contents, as_otel_part)
        chat_span$set_attribute(
          "gen_ai.system_instructions",
          jsonlite::toJSON(parts, auto_unbox = TRUE, null = "null")
        )
      }
      if (length(turns)) {
        # Tool result values are typed `class_any`, so tools can return objects
        # (environments, R6, external pointers) that `jsonlite::toJSON` rejects.
        # Skip emission rather than break the chat; the provider's tool_string
        # path will surface a descriptive error.
        tryCatch(
          {
            msgs <- lapply(turns, as_otel_message)
            chat_span$set_attribute(
              "gen_ai.input.messages",
              jsonlite::toJSON(msgs, auto_unbox = TRUE, null = "null")
            )
          },
          error = function(e) NULL
        )
      }
    }

    chat_span
  }

  # Starts an Open Telemetry span that abides by the semantic conventions for
  # Generative AI tool calls.
  #
  # Must be activated for the calling scope.
  #
  # See: https://opentelemetry.io/docs/specs/semconv/gen-ai/gen-ai-spans/#execute-tool-span
  local_tool_otel_span <<- function(
    request,
    parent = NULL,
    local_envir = parent.frame()
  ) {
    if (!otel_is_tracing) {
      return()
    }
    tool_span <-
      otel::start_span(
        sprintf("execute_tool %s", request@tool@name),
        options = list(parent = parent),
        attributes = compact(list(
          "gen_ai.operation.name" = "execute_tool",
          "gen_ai.tool.name" = request@tool@name,
          "gen_ai.tool.description" = request@tool@description,
          "gen_ai.tool.call.id" = request@id
        )),
        tracer = otel_tracer
      )

    setup_active_promise_otel_span(tool_span, local_envir)

    defer(otel::end_span(tool_span), envir = local_envir)

    tool_span
  }

  # Starts an Open Telemetry span that abides by the semantic conventions for
  # Generative AI "agents".
  #
  # See: https://opentelemetry.io/docs/specs/semconv/gen-ai/gen-ai-spans/#inference
  # local_otel_span_agent
  local_agent_otel_span <<- function(
    provider,
    model,
    activate = TRUE,
    conversation_id = NULL,
    local_envir = parent.frame()
  ) {
    if (!otel_is_tracing) {
      return()
    }
    if (activate) {
      abort(c(
        "Activating the agent span is not supported at this time.",
        "*" = "Activating the span here would set it as the active span globally (via otel::local_active_span() until the calling function ends (a long time).",
        "*" = "`coro::setup()` would address this and be appropriate",
        "i" = "Work around: Activate only where necessary or over a single yield in the calling scope."
      ))
    }
    agent_span <-
      otel::start_span(
        "invoke_agent",
        # TODO: "client" vs "internal" is under discussion, see #1146.
        options = list(kind = "client"),
        attributes = c(
          compact(list(
            "gen_ai.operation.name" = "invoke_agent",
            "gen_ai.provider.name" = otel_provider_name(provider),
            "gen_ai.request.model" = model@name,
            "gen_ai.conversation.id" = conversation_id
          )),
          otel_request_attributes(model)
        ),
        tracer = otel_tracer
      )

    ## Do not activate!
    ## The current usage of `local_agent_otel_span()` is in a multi-step coroutine.
    ## This would require deactivating only after the coroutine is done,
    ## but not between yields, that is too long and unpredictable.
    ## The span should only be activated in the specific steps where it is needed.
    # setup_active_promise_otel_span(agent_span, local_envir)

    defer(otel::end_span(agent_span), envir = local_envir)

    agent_span
  }
})

tracer_enabled <- function(tracer) {
  .subset2(tracer, "is_enabled")()
}

span_recording <- function(span) {
  .subset2(span, "is_recording")()
}

with_otel_record <- function(expr) {
  on.exit(otel_cache_tracer())
  otelsdk::with_otel_record({
    otel_cache_tracer()
    value <- expr
    # otelsdk (<= 0.2.4) only exports in-memory metrics on shutdown, which
    # happens after they are collected. Shut down early so they are returned.
    meter_provider <- otel::get_default_meter_provider()
    meter_provider$flush()
    meter_provider$shutdown()
    value
  })
}

# Flatten recorded metrics into a list of data points named by instrument.
otel_metric_points <- function(metrics) {
  scopes <- unlist(lapply(metrics, \(x) x$scope_metric_data), recursive = FALSE)
  data <- unlist(lapply(scopes, \(x) x$metric_data), recursive = FALSE)
  points <- lapply(data, function(metric) {
    lapply(metric$point_data_attr, function(point) {
      list(
        attributes = unclass(point$attributes),
        count = point$value$count,
        sum = point$value$sum
      )
    })
  })
  set_names(points, map_chr(data, \(x) x$instrument_name))
}

otel_metric_attributes <- function(provider, model, result = NULL) {
  c(
    list(
      "gen_ai.operation.name" = "chat",
      "gen_ai.provider.name" = otel_provider_name(provider),
      "gen_ai.request.model" = model@name,
      "gen_ai.response.model" = result$model
    ),
    otel_server_attributes(provider)
  )
}

elapsed_secs <- function(start) {
  as.numeric(Sys.time() - start, units = "secs")
}

record_chat_otel_span_status <- function(span, provider, model, result, start) {
  attributes <- otel_metric_attributes(provider, model, result)
  otel_record_histogram(
    "gen_ai.client.inference.duration",
    elapsed_secs(start),
    attributes
  )

  tokens <- value_tokens(provider, result)
  input <- as.integer(tokens$input + tokens$cached_input)
  output <- as.integer(tokens$output)
  if (input > 0L || output > 0L) {
    otel_record_histogram(
      "gen_ai.client.inference.operation.input_tokens",
      input,
      attributes
    )
    otel_record_histogram(
      "gen_ai.client.inference.operation.output_tokens",
      output,
      attributes
    )
  }

  if (is.null(span) || !span_recording(span)) {
    return()
  }
  if (!is.null(result$model)) {
    span$set_attribute("gen_ai.response.model", result$model)
  }
  if (!is.null(result$id)) {
    span$set_attribute("gen_ai.response.id", result$id)
  }
  if (input > 0L || output > 0L) {
    span$set_attribute("gen_ai.usage.input_tokens", input)
    span$set_attribute("gen_ai.usage.output_tokens", output)
  }
  if (tokens$cached_input > 0) {
    span$set_attribute(
      "gen_ai.usage.cache_read.input_tokens",
      as.integer(tokens$cached_input)
    )
  }
  reasoning <- value_reasoning_tokens(provider, result)
  if (!is.null(reasoning) && reasoning > 0) {
    span$set_attribute(
      "gen_ai.usage.reasoning.output_tokens",
      as.integer(reasoning)
    )
  }
  span$set_status("ok")
}

otel_error_type <- function(error) {
  class(error)[1L]
}

# Records a failed model request: the operation duration metric tagged with
# `error.type`, plus the exception and error status on the chat span. `start`
# is `NULL` once the duration has already been recorded for this request.
record_chat_otel_span_error <- function(span, provider, model, error, start) {
  if (!is.null(start)) {
    attributes <- otel_metric_attributes(provider, model)
    attributes[["error.type"]] <- otel_error_type(error)
    otel_record_histogram(
      "gen_ai.client.inference.duration",
      elapsed_secs(start),
      attributes
    )
  }
  record_otel_span_error(span, error)
  if (!is.null(span) && span_recording(span)) {
    span$set_attribute("gen_ai.response.finish_reasons", "error")
  }
}

# Convert a single Content into a GenAI semconv "part", a named list emitted
# as one entry of a ChatMessage's `parts` array. New content classes fall
# through to the default method, which emits a schema-valid `generic` part.
as_otel_part <- new_generic("as_otel_part", "content")

method(as_otel_part, Content) <- function(content) {
  list(type = "generic", class = S7_class(content)@name)
}

method(as_otel_part, ContentText) <- function(content) {
  list(type = "text", content = content@text)
}

method(as_otel_part, ContentToolRequest) <- function(content) {
  list(
    type = "tool_call",
    id = content@id,
    name = content@name,
    arguments = content@arguments
  )
}

method(as_otel_part, ContentToolResult) <- function(content) {
  part <- list(type = "tool_call_response")
  if (!is.null(content@request)) {
    part$id <- content@request@id
  }
  part$response <- tool_otel_response(content)
  part
}

tool_otel_response <- function(content) {
  if (tool_errored(content)) {
    return(tool_error_string(content))
  }
  value <- content@value
  if (inherits(value, "json")) {
    # Parse so jsonlite re-emits the structured value rather than encoding the
    # JSON string itself as a quoted string under `auto_unbox = TRUE`.
    return(jsonlite::fromJSON(value, simplifyVector = FALSE))
  }
  value
}

# Produce a GenAI semconv ChatMessage from a Turn. Tool-result UserTurns get
# role "tool" so consumers can filter them out of normal user input, matching
# Python's GenAI instrumentations.
as_otel_message <- function(turn) {
  list(
    role = if (is_tool_result_turn(turn)) "tool" else turn@role,
    parts = lapply(turn@contents, as_otel_part)
  )
}

otel_chat_input <- function(private, user_turn) {
  if (private$has_system_prompt()) {
    sys_turn <- private$.turns[[1]]
    history <- private$.turns[-1]
  } else {
    sys_turn <- NULL
    history <- private$.turns
  }
  list(
    turns = c(history, list(user_turn)),
    system_prompt = sys_turn
  )
}

# Records the time to first chunk (in seconds) as a chat span attribute and
# as the `gen_ai.client.inference.time_to_first_chunk` histogram. Per the
# client inference conventions, this is measured from request issuance to the
# first chunk received, whether or not that chunk contains model output.
# See: https://github.com/open-telemetry/semantic-conventions-genai/blob/main/docs/gen-ai/client-inference.md
record_chat_otel_ttft <- function(span, provider, model, start) {
  ttft <- elapsed_secs(start)
  otel_record_histogram(
    "gen_ai.client.inference.time_to_first_chunk",
    ttft,
    otel_metric_attributes(provider, model)
  )
  if (is.null(span) || !span_recording(span)) {
    return()
  }
  span$set_attribute("gen_ai.response.time_to_first_chunk", ttft)
}

record_chat_otel_span_output <- function(span, turn) {
  if (is.null(span) || !span_recording(span)) {
    return()
  }
  if (!S7_inherits(turn, AssistantTurn)) {
    return()
  }
  # Per semconv, a cancelled or interrupted stream reports an `error` finish
  # reason rather than omitting it. The convention types this as `string[]`,
  # but otel can't record a length-1 vector as an array, so a single finish
  # reason is exported as a scalar. See r-lib/otel#48.
  if (is_partial_turn(turn)) {
    span$set_attribute("gen_ai.response.finish_reasons", "error")
  } else if (!is.na(turn@finish_reason)) {
    span$set_attribute("gen_ai.response.finish_reasons", turn@finish_reason)
  }
  if (!otel_capture_content_enabled()) {
    return()
  }
  msg <- as_otel_message(turn)
  span$set_attribute(
    "gen_ai.output.messages",
    jsonlite::toJSON(list(msg), auto_unbox = TRUE, null = "null")
  )
}

# Per-invocation counts backing the invoke_agent span attributes and metrics.
new_agent_otel_tally <- function() {
  env(
    start = Sys.time(),
    inference_calls = 0L,
    tool_calls = 0L,
    tokens = c(0, 0, 0),
    error = NULL
  )
}

# Inference calls are counted when the request starts (so failed requests are
# included); tool calls and tokens come from the completed turn.
tally_agent_otel_request <- function(tally) {
  tally$inference_calls <- tally$inference_calls + 1L
  invisible(tally)
}

tally_agent_otel_turn <- function(tally, turn) {
  tally$tool_calls <- tally$tool_calls + length(extract_tool_requests(turn))
  # tokens are c(input, output, cached_input)
  tally$tokens <- tally$tokens + ifelse(is.na(turn@tokens), 0, turn@tokens)
  invisible(tally)
}

# Records the invoke_agent metrics and, mirroring the chat span, also sets
# them as attributes on the invoke_agent span.
record_agent_otel <- function(span, provider, model, tally) {
  attributes <- otel_metric_attributes(provider, model)
  attributes[["gen_ai.operation.name"]] <- "invoke_agent"
  error_type <- if (!is.null(tally$error)) otel_error_type(tally$error)
  values <- list(
    "gen_ai.invoke_agent.duration" = elapsed_secs(tally$start),
    "gen_ai.invoke_agent.inference_calls" = tally$inference_calls,
    "gen_ai.invoke_agent.tool_calls" = tally$tool_calls
  )
  # Only the duration metric defines `error.type` in the semantic conventions.
  otel_record_histogram(
    "gen_ai.invoke_agent.duration",
    values[[1L]],
    c(attributes, "error.type" = error_type)
  )
  for (name in names(values)[-1L]) {
    otel_record_histogram(name, values[[name]], attributes)
  }

  if (is.null(span) || !span_recording(span)) {
    return()
  }
  for (name in names(values)) {
    span$set_attribute(name, values[[name]])
  }
  if (!is.null(error_type)) {
    # The exception event is recorded once, on the chat or tool span where it
    # occurred; the agent span only carries the status and error type.
    span$set_status("error")
    span$set_attribute("error.type", error_type)
  }
  input <- as.integer(tally$tokens[[1]] + tally$tokens[[3]])
  output <- as.integer(tally$tokens[[2]])
  if (input > 0L || output > 0L) {
    span$set_attribute("gen_ai.usage.input_tokens", input)
    span$set_attribute("gen_ai.usage.output_tokens", output)
  }
}

record_tool_otel_duration <- function(request, start, result) {
  otel_record_histogram(
    "gen_ai.execute_tool.duration",
    elapsed_secs(start),
    list(
      "gen_ai.operation.name" = "execute_tool",
      "gen_ai.tool.name" = request@tool@name,
      "gen_ai.tool.type" = "function",
      "error.type" = if (tool_errored(result)) tool_error_type(result)
    )
  )
}

tool_error_type <- function(result) {
  if (is.character(result@error)) "error" else class(result@error)[1L]
}

record_otel_span_error <- function(span, error) {
  if (is.null(span) || !span_recording(span)) {
    return()
  }
  span$record_exception(error)
  span$set_status("error")
  span$set_attribute("error.type", otel_error_type(error))
}

# Only activate the span if it is non-NULL. If
# otel_promise_domain is TRUE, also ensure that the active span is reactivated upon promise domain restoration.
#' Activate and use handoff promise domain for Open Telemetry span
#'
#' @param otel_span An Open Telemetry span object.
#' @param activation_scope The scope in which to activate the span.
#' @noRd
setup_active_promise_otel_span <- function(
  span,
  activation_scope = parent.frame()
) {
  if (is.null(span) || !span_recording(span)) {
    return()
  }

  promises::local_otel_promise_domain(activation_scope)
  otel::local_active_span(span, activation_scope = activation_scope)

  invisible()
}
