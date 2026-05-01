otel_tracer_name <- "co.posit.r-package.ellmer"

otel_cache_tracer <- NULL
otel_capture_content_enabled <- NULL
local_chat_otel_span <- NULL
local_tool_otel_span <- NULL
local_agent_otel_span <- NULL

local({
  otel_is_tracing <- FALSE
  otel_tracer <- NULL
  otel_capture_content <- FALSE

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
  }

  otel_capture_content_enabled <<- function() otel_capture_content

  local_chat_otel_span <<- function(
    provider,
    turns = NULL,
    system_prompt = NULL,
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
        # Skip emission rather than break the chat — the provider's tool_string
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
          "gen_ai.operation.name" = "invoke_agent",
          "gen_ai.provider.name" = tolower(provider@name),
          "gen_ai.request.model" = provider@model
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

record_chat_otel_span_status <- function(span, provider, result) {
  if (is.null(span) || !span_recording(span)) {
    return()
  }
  if (!is.null(result$model)) {
    span$set_attribute("gen_ai.response.model", result$model)
  }
  if (!is.null(result$id)) {
    span$set_attribute("gen_ai.response.id", result$id)
  }
  tokens <- value_tokens(provider, result)
  input <- as.integer(tokens$input + tokens$cached_input)
  output <- as.integer(tokens$output)
  if (input > 0L || output > 0L) {
    span$set_attribute("gen_ai.usage.input_tokens", input)
    span$set_attribute("gen_ai.usage.output_tokens", output)
  }
  # TODO: Consider setting gen_ai.response.finish_reasons.
  span$set_status("ok")
}

# Convert a single Content into a GenAI semconv "part" — a named list emitted
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
# role "tool" so consumers can filter them out of normal user input — matching
# Python's GenAI instrumentations.
as_otel_message <- function(turn) {
  list(
    role = if (is_tool_result_turn(turn)) "tool" else turn@role,
    parts = lapply(turn@contents, as_otel_part)
  )
}

is_tool_result_turn <- function(turn) {
  S7_inherits(turn, UserTurn) &&
    length(turn@contents) > 0 &&
    all(map_lgl(turn@contents, S7_inherits, ContentToolResult))
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

record_chat_otel_span_output <- function(span, turn) {
  if (is.null(span) || !span_recording(span)) {
    return()
  }
  if (!otel_capture_content_enabled()) {
    return()
  }
  if (!S7_inherits(turn, AssistantTurn)) {
    return()
  }
  msg <- as_otel_message(turn)
  span$set_attribute(
    "gen_ai.output.messages",
    jsonlite::toJSON(list(msg), auto_unbox = TRUE, null = "null")
  )
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
