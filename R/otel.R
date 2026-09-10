otel_tracer_name <- "co.posit.r-package.ellmer"

otel_cache_tracer <- NULL
otel_capture_content_enabled <- NULL
local_chat_otel_span <- NULL
local_tool_otel_span <- NULL
local_agent_otel_span <- NULL
otel_record_histogram <- NULL

# Histograms from the GenAI semantic conventions for metrics.
# See: https://opentelemetry.io/docs/specs/semconv/gen-ai/gen-ai-metrics/
otel_histogram_specs <- list(
  "gen_ai.client.operation.duration" = list(
    description = "GenAI operation duration",
    unit = "s"
  ),
  "gen_ai.client.token.usage" = list(
    description = "Number of input and output tokens used",
    unit = "{token}"
  ),
  "gen_ai.client.operation.time_to_first_chunk" = list(
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

# Map `params()` onto the `gen_ai.request.*` span attributes.
otel_request_attributes <- function(model) {
  p <- model@params
  compact(list(
    "gen_ai.request.temperature" = p$temperature,
    "gen_ai.request.top_p" = p$top_p,
    "gen_ai.request.top_k" = p$top_k,
    "gen_ai.request.frequency_penalty" = p$frequency_penalty,
    "gen_ai.request.presence_penalty" = p$presence_penalty,
    "gen_ai.request.seed" = p$seed,
    "gen_ai.request.max_tokens" = p$max_tokens,
    "gen_ai.request.stop_sequences" = p$stop_sequences
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
            "gen_ai.provider.name" = tolower(provider@name),
            "gen_ai.request.model" = model@name,
            "gen_ai.conversation.id" = conversation_id,
            # Only set when streaming; unset means non-streaming per semconv.
            "gen_ai.request.stream" = if (stream) TRUE
          )),
          otel_request_attributes(model)
        ),
        tracer = otel_tracer
      )

    defer(otel::end_span(chat_span), envir = local_envir)

    if (otel_capture_content) {
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
            "gen_ai.provider.name" = tolower(provider@name),
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
  list(
    "gen_ai.operation.name" = "chat",
    "gen_ai.provider.name" = tolower(provider@name),
    "gen_ai.request.model" = model@name,
    "gen_ai.response.model" = result$model
  )
}

elapsed_secs <- function(start) {
  as.numeric(Sys.time() - start, units = "secs")
}

record_chat_otel_span_status <- function(span, provider, model, result, start) {
  attributes <- otel_metric_attributes(provider, model, result)
  otel_record_histogram(
    "gen_ai.client.operation.duration",
    elapsed_secs(start),
    attributes
  )

  tokens <- value_tokens(provider, result)
  input <- as.integer(tokens$input + tokens$cached_input)
  output <- as.integer(tokens$output)
  if (input > 0L || output > 0L) {
    otel_record_histogram(
      "gen_ai.client.token.usage",
      input,
      c(attributes, "gen_ai.token.type" = "input")
    )
    otel_record_histogram(
      "gen_ai.client.token.usage",
      output,
      c(attributes, "gen_ai.token.type" = "output")
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
  span$set_status("ok")
}

otel_error_type <- function(error) {
  class(error)[1L]
}

# Records a failed model request: the operation duration metric tagged with
# `error.type`, plus the exception and error status on the chat span.
record_chat_otel_span_error <- function(span, provider, model, error, start) {
  attributes <- otel_metric_attributes(provider, model)
  attributes[["error.type"]] <- otel_error_type(error)
  otel_record_histogram(
    "gen_ai.client.operation.duration",
    elapsed_secs(start),
    attributes
  )
  record_otel_span_error(span, error)
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

# Records the time to first token (in seconds) as a chat span attribute and
# as the `gen_ai.client.operation.time_to_first_chunk` histogram.
record_chat_otel_ttft <- function(span, provider, model, start) {
  ttft <- elapsed_secs(start)
  otel_record_histogram(
    "gen_ai.client.operation.time_to_first_chunk",
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
  if (!is.na(turn@finish_reason)) {
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

tally_agent_otel_turn <- function(tally, turn) {
  tally$inference_calls <- tally$inference_calls + 1L
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
  if (!is.null(tally$error)) {
    attributes[["error.type"]] <- otel_error_type(tally$error)
  }
  values <- list(
    "gen_ai.invoke_agent.duration" = elapsed_secs(tally$start),
    "gen_ai.invoke_agent.inference_calls" = tally$inference_calls,
    "gen_ai.invoke_agent.tool_calls" = tally$tool_calls
  )
  for (name in names(values)) {
    otel_record_histogram(name, values[[name]], attributes)
  }

  if (is.null(span) || !span_recording(span)) {
    return()
  }
  for (name in names(values)) {
    span$set_attribute(name, values[[name]])
  }
  if (!is.null(tally$error)) {
    record_otel_span_error(span, tally$error)
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
