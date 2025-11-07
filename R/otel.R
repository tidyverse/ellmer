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

otel_is_enabled <- function(tracer = ellmer_otel_tracer()) {
  .subset2(tracer, "is_enabled")()
}


# Only activate the span if it is non-NULL. If
# otel_promise_domain is TRUE, also ensure that the active span is reactivated upon promise domain restoration.
#' Activate and use handoff promise domain for Open Telemetry span
#'
#' @param otel_span An Open Telemetry span object.
#' @param activation_scope The scope in which to activate the span.
#' @noRd
setup_active_promise_otel_span <- function(
  otel_span,
  activation_scope = parent.frame()
) {
  if (is.null(otel_span) || !otel_is_enabled()) {
    return()
  }

  promises::local_otel_promise_domain(activation_scope)
  otel::local_active_span(
    otel_span,
    activation_scope = activation_scope
  )

  invisible()
}


local_chat_otel_span <- function(
  provider,
  parent = NULL,
  local_envir = parent.frame(),
  tracer = ellmer_otel_tracer()
) {
  chat_span <-
    otel::start_span(
      sprintf("chat %s", provider@model),
      tracer = tracer,
      options = list(
        parent = parent,
        kind = "CLIENT"
      ),
      attributes = list(
        "gen_ai.operation.name" = "chat",
        "gen_ai.provider.name" = tolower(provider@name),
        "gen_ai.request.model" = provider@model
      )
    )

  defer(otel::end_span(chat_span), envir = local_envir)

  chat_span
}

record_chat_otel_span_status <- function(span, result) {
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
local_tool_otel_span <- function(
  request,
  parent = NULL,
  local_envir = parent.frame(),
  tracer = ellmer_otel_tracer()
) {
  tool_span <-
    otel::start_span(
      sprintf("execute_tool %s", request@tool@name),
      tracer = tracer,
      options = list(
        parent = parent,
        kind = "INTERNAL"
      ),
      attributes = compact(list(
        "gen_ai.operation.name" = "execute_tool",
        "gen_ai.tool.name" = request@tool@name,
        "gen_ai.tool.description" = request@tool@description,
        "gen_ai.tool.call.id" = request@id
      ))
    )

  setup_active_promise_otel_span(tool_span, local_envir)

  defer(otel::end_span(tool_span), envir = local_envir)

  tool_span
}

record_tool_otel_span_error <- function(otel_span, error) {
  if (is.null(otel_span) || !otel_span$is_recording()) {
    return()
  }
  otel_span$record_exception(error)
  otel_span$set_status("error")
  otel_span$set_attribute("error.type", class(error)[1])
}


# Starts an Open Telemetry span that abides by the semantic conventions for
# Generative AI "agents".
#
# See: https://opentelemetry.io/docs/specs/semconv/gen-ai/gen-ai-spans/#inference
# local_otel_span_agent
local_agent_otel_span <- function(
  provider,
  tracer = ellmer_otel_tracer(),
  local_envir = parent.frame()
) {
  agent_span <-
    otel::start_span(
      "invoke_agent",
      tracer = tracer,
      options = list(kind = "CLIENT"),
      attributes = list(
        "gen_ai.operation.name" = "chat",
        "gen_ai.provider.name" = tolower(provider@name)
      )
    )

  setup_active_promise_otel_span(agent_span, local_envir)

  defer(otel::end_span(agent_span), envir = local_envir)

  agent_span
}
