otel_tracer_name <- "co.posit.r-package.ellmer"

otel_cache_tracer <- NULL
local_chat_otel_span <- NULL
local_tool_otel_span <- NULL
local_agent_otel_span <- NULL

local({
  otel_is_tracing <- FALSE
  otel_tracer <- NULL

  otel_cache_tracer <<- function() {
    if (!requireNamespace("otel", quietly = TRUE)) {
      return()
    }
    otel_tracer <<- otel::get_tracer(otel_tracer_name)
    otel_is_tracing <<- tracer_enabled(otel_tracer)
  }

  local_chat_otel_span <<- function(
    provider,
    parent = NULL,
    local_envir = parent.frame()
  ) {
    if (!otel_is_tracing) {
      return()
    }
    chat_span <-
      otel::start_span(
        sprintf("chat %s", provider@model),
        options = list(
          parent = parent,
          kind = "client"
        ),
        attributes = list(
          "gen_ai.operation.name" = "chat",
          "gen_ai.provider.name" = tolower(provider@name),
          "gen_ai.request.model" = provider@model
        ),
        tracer = otel_tracer
      )

    defer(otel::end_span(chat_span), envir = local_envir)

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
    activate = TRUE,
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
        options = list(kind = "client"),
        attributes = list(
          "gen_ai.operation.name" = "chat",
          "gen_ai.provider.name" = tolower(provider@name)
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
    expr
  })
}

record_chat_otel_span_status <- function(span, result) {
  if (is.null(span) || !span_recording(span)) {
    return()
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

record_tool_otel_span_error <- function(span, error) {
  if (is.null(span) || !span_recording(span)) {
    return()
  }
  span$record_exception(error)
  span$set_status("error")
  span$set_attribute("error.type", class(error)[1L])
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
