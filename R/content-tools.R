#' @include turns.R
NULL

# Results a content list
invoke_tools <- function(turn, echo = "none") {
  tool_requests <- extract_tool_requests(turn@contents)

  lapply(tool_requests, function(request) {
    maybe_echo_tool(request, echo = echo)
    result <- invoke_tool(request)

    if (promises::is.promise(result@value)) {
      cli::cli_abort(c(
        "Can't use async tools with `$chat()` or `$stream()`.",
        i = "Async tools are supported, but you must use `$chat_async()` or `$stream_async()`."
      ))
    }

    maybe_echo_tool(result, echo = echo)
    result
  })
}

on_load(
  invoke_tools_async <- coro::async(function(turn, tools, echo = "none") {
    tool_requests <- extract_tool_requests(turn@contents)

    # We call it this way instead of a more natural for + await_each() because
    # we want to run all the async tool calls in parallel
    result_promises <- lapply(tool_requests, function(request) {
      maybe_echo_tool(request, echo = echo)

      invoke_tool_async(request)
    })

    result_promises <- lapply(result_promises, function(p) {
      p$then(function(result) {
        maybe_echo_tool(result, echo = echo)
      })
    })

    promises::promise_all(.list = result_promises)
  })
)

extract_tool_requests <- function(contents) {
  is_tool_request <- map_lgl(contents, S7_inherits, ContentToolRequest)
  contents[is_tool_request]
}

new_tool_result <- function(request, result = NULL, error = NULL) {
  check_exclusive(result, error)

  if (!is.null(error)) {
    ContentToolResult(error = error, request = request)
  } else if (S7_inherits(result, ContentToolResult)) {
    set_props(result, request = request)
  } else {
    ContentToolResult(value = result, request = request)
  }
}

# Also need to handle edge cases: https://platform.openai.com/docs/guides/function-calling/edge-cases
invoke_tool <- function(request) {
  if (is.null(request@tool)) {
    return(new_tool_result(request, error = "Unknown tool"))
  }

  tryCatch(
    {
      result <- do.call(request@tool@fun, request@arguments)
      new_tool_result(request, result)
    },
    error = function(e) {
      # TODO: We need to report this somehow; it's way too hidden from the user
      new_tool_result(request, error = e)
    }
  )
}

on_load(
  invoke_tool_async <- coro::async(function(request) {
    if (is.null(request@tool)) {
      return(new_tool_result(request, error = "Unknown tool"))
    }

    tryCatch(
      {
        result <- await(do.call(request@tool@fun, request@arguments))
        new_tool_result(request, result)
      },
      error = function(e) {
        # TODO: We need to report this somehow; it's way too hidden from the user
        new_tool_result(request, error = e)
      }
    )
  })
)

turn_get_tool_errors <- function(turn = NULL) {
  if (is.null(turn)) return(NULL)
  stopifnot(S7_inherits(turn, Turn))

  if (length(turn@contents) == 0) {
    return(NULL)
  }

  is_result <- map_lgl(turn@contents, S7_inherits, ContentToolResult)
  if (!any(is_result)) {
    return(NULL)
  }

  is_error <- map_lgl(turn@contents[is_result], tool_errored)

  res <- turn@contents[is_result][is_error]
  if (length(res)) res else NULL
}

warn_tool_errors <- function(tool_errors) {
  # tool_errors is a list of errors returned from turn_get_tool_errors()
  if (length(tool_errors) > 0) {
    errs <- map_chr(
      tool_errors[seq_len(min(3, length(tool_errors)))],
      function(result) {
        name <- result@request@name %||% "unknown_tool"
        id <- result@request@id
        error <- tool_error_string(result)
        cli::format_inline("[{.field {name}} ({id})]: {error}")
      }
    )
    cli::cli_warn(c(
      "Failed to evaluate {length(tool_errors)} tool call{?s}.",
      set_names(errs, "i"),
      "i" = if (length(errs) < length(tool_errors)) {
        cli::format_inline(
          "... and {length(tool_errors) - length(errs)} more"
        )
      }
    ))
  }
}

maybe_echo_tool <- function(x, echo = "output") {
  if (!identical(echo, "output")) {
    return(invisible(x))
  }

  cli_escape <- function(x) {
    x <- gsub("{", "{{", x, fixed = TRUE)
    gsub("}", "}}", x, fixed = TRUE)
  }

  if (S7_inherits(x, ContentToolRequest)) {
    cli::cli_alert(cli_escape(format(x)))
    return(invisible(x))
  }

  if (!S7_inherits(x, ContentToolResult)) {
    # neither tool result or request
    return(invisible(x))
  }

  # ContentToolResult ----
  if (tool_errored(x)) {
    alert <- cli::cli_alert_warning
    value <- tool_error_string(x)
  } else {
    alert <- cli::cli_alert_success
    value <- tool_string(x)
  }

  header <- cli_escape(format(x, show = "header"))
  # force two spaces between result and ID (will be removed by cli_alert if not)
  header <- sub("  ", "\u00a0\u00a0", header, fixed = TRUE)

  is_short_line <- !grepl("\n", value)

  if (tool_errored(x)) {
    if (is_short_line) {
      value <- paste(cli::col_red("Error:"), value)
    } else {
      header <- paste(header, cli::col_red("Tool calling failed with error:"))
    }
  }

  if (is_short_line) {
    alert(cli_escape(paste(header, value)))
  } else {
    alert(header)
    cli::cli_div(
      class = "padded",
      theme = list(.padded = list("margin-left" = 2))
    )
    cli::cli_verbatim(cli::style_italic(value))
    cli::cli_verbatim("")
    cli::cli_end()
  }

  invisible(x)
}
