# Starts an Open Telemetry span that abides by the semantic conventions for
# Generative AI clients.
#
# See: https://opentelemetry.io/docs/specs/semconv/gen-ai/gen-ai-spans/
start_chat_span <- function(provider, tracer = NULL, scope = parent.frame()) {
  if (!is_installed("otel")) {
    return(NULL)
  }
  tracer <- tracer %||% otel::get_tracer("ellmer")
  name <- sprintf("chat %s", provider@model)
  if (!tracer$is_enabled()) {
    # Return a no-op span when tracing is disabled.
    return(tracer$start_span(name))
  }
  tracer$start_span(
    name,
    options = list(kind = "CLIENT"),
    # Ensure we set attributes relevant to sampling at span creation time.
    attributes = compact(list(
      "gen_ai.operation.name" = "chat",
      "gen_ai.system" = tolower(provider@name),
      "gen_ai.request.model" = provider@model
    )),
    scope = scope
  )
}

end_chat_span <- function(span, result) {
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
  span$end()
}
