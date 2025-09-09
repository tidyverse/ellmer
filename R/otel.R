default_tracer <- function() {
  if (!is_installed("otel")) {
    return(NULL)
  }
  otel::get_tracer("ellmer")
}

activate_and_cleanup_ospan <- function(
  ospan,
  activation_scope = parent.frame(),
  ospan_promise_domain = TRUE
) {
  if (!is.null(ospan)) {
    if (ospan_promise_domain) {
      local_ospan_promise_domain()
    }
    otel::local_active_span(
      ospan,
      end_on_exit = TRUE,
      activation_scope = activation_scope
    )
  }
}

local_ospan_promise_domain <- function(.local_envir = parent.frame()) {
  local_promise_domain(
    promises:::create_otel_ospan_handoff_promise_domain(),
    .local_envir = .local_envir
  )
}

local_promise_domain <- function(
  domain,
  .local_envir = parent.frame(),
  replace = FALSE
) {
  oldval <- promises:::current_promise_domain()
  globals <- promises:::globals
  if (replace) {
    globals$domain <- domain
  } else {
    globals$domain <- promises:::compose_domains(oldval, domain)
  }
  withr::defer(
    {
      globals$domain <- oldval
    },
    envir = .local_envir
  )

  invisible()
}

create_chat_ospan <- function(
  provider,
  parent_ospan = NULL,
  tracer = default_tracer()
) {
  promises::create_ospan(
    sprintf("chat %s", provider@model),
    tracer = tracer,
    options = list(
      parent = parent_ospan,
      kind = "CLIENT"
    ),
    attributes = list(
      "gen_ai.operation.name" = "chat",
      "gen_ai.system" = tolower(provider@name),
      "gen_ai.request.model" = provider@model
    )
  )
}

# # Starts an Open Telemetry span that abides by the semantic conventions for
# # Generative AI completions.
# #
# # See: https://opentelemetry.io/docs/specs/semconv/gen-ai/gen-ai-spans/#inference
# with_chat_ospan_async <- function(provider, expr, tracer = default_tracer()) {
#   promises::with_ospan_promise_domain({
#     promises::with_ospan_async(
#       sprintf("chat %s", provider@model),
#       expr,
#       tracer = tracer,
#       options = list(kind = "CLIENT"),
#       attributes = list(
#         "gen_ai.operation.name" = "chat",
#         "gen_ai.system" = tolower(provider@name),
#         "gen_ai.request.model" = provider@model
#       )
#     )
#   })
# }

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
# See: https://opentelemetry.io/docs/specs/semconv/gen-ai/gen-ai-spans/#execute-tool-span
create_tool_ospan <- function(
  request,
  parent_ospan = NULL,
  tracer = default_tracer()
) {
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
}

# with_tool_ospan_async <- function(request, expr, tracer = default_tracer()) {
#   promises::with_ospan_promise_domain({
#     promises::with_ospan_async(
#       sprintf("execute_tool %s", request@tool@name),
#       expr,
#       tracer = tracer,
#       options = list(kind = "INTERNAL"),
#       attributes = compact(list(
#         "gen_ai.operation.name" = "execute_tool",
#         "gen_ai.tool.name" = request@tool@name,
#         "gen_ai.tool.description" = request@tool@description,
#         "gen_ai.tool.call.id" = request@id
#       ))
#     )
#   })
# }

record_tool_ospan_error <- function(span, error) {
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
create_agent_ospan <- function(
  provider,
  tracer = default_tracer()
) {
  promises::create_ospan(
    "invoke_agent",
    tracer = tracer,
    options = list(kind = "CLIENT"),
    attributes = list(
      "gen_ai.operation.name" = "chat",
      "gen_ai.system" = tolower(provider@name)
    )
  )
}

# with_agent_ospan_async <- function(provider, expr, tracer = default_tracer()) {
#   promises::with_ospan_promise_domain({
#     promises::with_ospan_async(
#       "invoke_agent",
#       expr,
#       tracer = tracer,
#       options = list(kind = "CLIENT"),
#       attributes = list(
#         "gen_ai.operation.name" = "chat",
#         "gen_ai.system" = tolower(provider@name)
#       )
#     )
#   })
# }

# ------------------------

gen_adapt_map <- coro::generator(function(.i, .fn, ...) {
  for (x in .i) {
    yield(.fn(x, ...))
  }
})


# prev_gen %>% gen_adapt_ospan(chat_ospan)
gen_adapt_ospan <- function(x, ospan) {
  gen_adapt_map(x, function(xi) {
    local_ospan_promise_domain()
    otel::local_active_span(ospan)
    message("Activated ospan.   : ", ospan$name, " - ", ospan$span_id)
    withr::defer(message(
      "deactivating ospan : ",
      ospan$name,
      " - ",
      ospan$span_id
    ))
    force(xi)
  })
}
