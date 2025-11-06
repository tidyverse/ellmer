otel_tracer_name <- "co.posit.r-package.ellmer"

# Inspired by httr2:::get_tracer() / shiny:::get_tracer()
# Using local scope avoids an environment object lookup on each call.
ellmer_otel_tracer <- local({
  tracer <- NULL
  function() {
    if (!is.null(tracer)) {
      return(tracer)
    }
    if (is_testing()) {
      # Don't cache the tracer in unit tests. It interferes with tracer provider
      # injection in otelsdk::with_otel_record().
      return(otel::get_tracer())
    }
    tracer <<- otel::get_tracer()
    tracer
  }
})


# Only activate the span if it is non-NULL. If activated, ensure it is
# automatically ended when the activation scope exits. If
# ospan_promise_domain is TRUE, also ensure that the active span is reactivated upon promise domain restoration.
setup_otel_span <- function(
  ospan,
  activation_scope = parent.frame(),
  ospan_promise_domain = TRUE
) {
  if (!is.null(ospan)) {
    if (ospan_promise_domain) {
      promises::local_ospan_promise_domain(activation_scope)
    }
    otel::local_active_span(
      ospan,
      end_on_exit = FALSE,
      activation_scope = activation_scope
    )
    # TODO: Set status?
    defer(promises::end_ospan(ospan), envir = activation_scope)
  }

  invisible(ospan)
}


local_chat_ospan <- function(
  provider,
  parent_ospan = NULL,
  local_envir = parent.frame(),
  tracer = ellmer_otel_tracer()
) {
  chat_ospan <-
    promises::create_ospan(
      sprintf("chat %s", provider@model),
      tracer = tracer,
      options = list(
        parent = parent_ospan,
        kind = "CLIENT"
      ),
      attributes = list(
        "gen_ai.operation.name" = "chat",
        "gen_ai.provider.name" = tolower(provider@name),
        "gen_ai.request.model" = provider@model
      )
    )

  defer(promises::end_ospan(chat_ospan), envir = local_envir)

  chat_ospan
}

record_chat_ospan_status <- function(span, result) {
  if (is.null(span) || !span$is_recording()) {
    return(invisible(span))
  }
  if (!is.null(result$model)) {
    span$set_attribute("gen_ai.response.model", result$model)
  }
  if (!is.null(result$id)) {
    span$set_attribute("gen_ai.response.id", result$id)
  }
  # TODO: Fixme @atheriel!
  # if (!is.null(result$usage)) {
  #   span$set_attribute("gen_ai.usage.input_tokens", result$usage$prompt_tokens)
  #   span$set_attribute(
  #     "gen_ai.usage.output_tokens",
  #     result$usage$completion_tokens
  #   )
  # }
  # TODO: Consider setting gen_ai.response.finish_reasons.
  span$set_status("ok")
}


# Starts an Open Telemetry span that abides by the semantic conventions for
# Generative AI tool calls.
#
# Must be activated for the calling scope.
#
# See: https://opentelemetry.io/docs/specs/semconv/gen-ai/gen-ai-spans/#execute-tool-span
start_local_active_tool_ospan <- function(
  request,
  parent_ospan = NULL,
  local_envir = parent.frame(),
  tracer = ellmer_otel_tracer()
) {
  tool_ospan <-
    promises::create_ospan(
      sprintf("execute_tool %s", request@tool@name),
      tracer = tracer,
      options = list(
        parent = parent_ospan,
        kind = "INTERNAL"
      ),
      attributes = compact(list(
        "gen_ai.operation.name" = "execute_tool",
        "gen_ai.tool.name" = request@tool@name,
        "gen_ai.tool.description" = request@tool@description,
        "gen_ai.tool.call.id" = request@id
      ))
    )

  setup_otel_span(tool_ospan, local_envir)

  tool_ospan
}

record_tool_ospan_error <- function(ospan, error) {
  if (is.null(ospan) || !ospan$is_recording()) {
    return()
  }
  ospan$record_exception(error)
  ospan$set_status("error")
  ospan$set_attribute("error.type", class(error)[1])
}


# Starts an Open Telemetry span that abides by the semantic conventions for
# Generative AI "agents".
#
# See: https://opentelemetry.io/docs/specs/semconv/gen-ai/gen-ai-spans/#inference
local_agent_ospan <- function(
  provider,
  tracer = ellmer_otel_tracer(),
  local_envir = parent.frame()
) {
  agent_ospan <-
    promises::create_ospan(
      "invoke_agent",
      tracer = tracer,
      options = list(kind = "CLIENT"),
      attributes = list(
        "gen_ai.operation.name" = "chat",
        "gen_ai.provider.name" = tolower(provider@name)
      )
    )

  defer(promises::end_ospan(agent_ospan), envir = local_envir)

  agent_ospan
}
