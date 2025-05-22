# Starts an Open Telemetry span that abides by the semantic conventions for
# Generative AI completions.
#
# See: https://opentelemetry.io/docs/specs/semconv/gen-ai/gen-ai-spans/#inference
start_chat_span <- function(
  provider,
  tracer = default_tracer(),
  scope = parent.frame(),
  active = TRUE
) {
  if (is.null(tracer) || !tracer$is_enabled()) {
    return(NULL)
  }
  # Ensure we set attributes relevant to sampling at span creation time.
  attributes <- list(
    "gen_ai.operation.name" = "chat",
    "gen_ai.system" = tolower(provider@name),
    "gen_ai.request.model" = provider@model
  )
  if (active) {
    tracer$start_span(
      name = sprintf("chat %s", provider@model),
      options = list(kind = "CLIENT"),
      attributes = attributes,
      scope = scope
    )
  } else {
    tracer$start_session(
      name = sprintf("chat %s", provider@model),
      options = list(kind = "CLIENT"),
      attributes = attributes,
      session_scope = scope
    )
  }
}

record_chat_span_status <- function(span, result) {
  if (is.null(span) || !span$is_recording()) {
    return(invisible(span))
  }
  if (!is.null(result$model)) {
    span$set_attribute("gen_ai.response.model", result$model)
  }
  if (!is.null(result$id)) {
    span$set_attribute("gen_ai.response.id", result$id)
  }
  if (!is.null(result$usage)) {
    span$set_attribute("gen_ai.usage.input_tokens", result$usage$prompt_tokens)
    span$set_attribute(
      "gen_ai.usage.output_tokens",
      result$usage$completion_tokens
    )
  }
  # TODO: Consider setting gen_ai.response.finish_reasons.
  span$set_status("ok")
}

# Starts an Open Telemetry span that abides by the semantic conventions for
# Generative AI tool calls.
#
# See: https://opentelemetry.io/docs/specs/semconv/gen-ai/gen-ai-spans/#execute-tool-span
start_tool_span <- function(
  request,
  tracer = default_tracer(),
  scope = parent.frame(),
  active = TRUE
) {
  if (is.null(tracer) || !tracer$is_enabled()) {
    return(NULL)
  }
  attributes <- compact(list(
    "gen_ai.operation.name" = "execute_tool",
    "gen_ai.tool.name" = request@tool@name,
    "gen_ai.tool.description" = request@tool@description,
    "gen_ai.tool.call.id" = request@id
  ))
  if (active) {
    tracer$start_span(
      name = sprintf("execute_tool %s", request@tool@name),
      options = list(kind = "INTERNAL"),
      attributes = attributes,
      scope = scope
    )
  } else {
    tracer$start_session(
      name = sprintf("execute_tool %s", request@tool@name),
      options = list(kind = "INTERNAL"),
      attributes = attributes,
      session_scope = scope
    )
  }
}

record_tool_error <- function(span, error) {
  if (is.null(span) || !span$is_recording()) {
    return()
  }
  span$record_exception(error)
  span$set_status("error")
  span$set_attribute("error.type", class(error)[1])
}

# Starts an Open Telemetry span that abides by the semantic conventions for
# Generative AI "agents".
#
# See: https://opentelemetry.io/docs/specs/semconv/gen-ai/gen-ai-spans/#inference
start_agent_span <- function(
  provider,
  tracer = default_tracer(),
  scope = parent.frame(),
  active = TRUE
) {
  if (is.null(tracer) || !tracer$is_enabled()) {
    return(NULL)
  }
  attributes <- list(
    "gen_ai.operation.name" = "chat",
    "gen_ai.system" = tolower(provider@name)
  )
  if (active) {
    tracer$start_span(
      name = "invoke_agent",
      options = list(kind = "CLIENT"),
      attributes = attributes,
      scope = scope
    )
  } else {
    tracer$start_session(
      name = "invoke_agent",
      options = list(kind = "CLIENT"),
      attributes = attributes,
      session_scope = scope
    )
  }
}

default_tracer <- function() {
  if (!is_installed("otel")) {
    return(NULL)
  }
  otel::get_tracer("ellmer")
}
