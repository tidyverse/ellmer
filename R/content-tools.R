#' @include turns.R
NULL

match_tools <- function(turn, tools) {
  if (is.null(turn)) return(NULL)

  turn@contents <- map(turn@contents, function(content) {
    if (!S7_inherits(content, ContentToolRequest)) {
      return(content)
    }
    content@tool <- tools[[content@name]]
    content
  })

  turn
}

on_load({
  invoke_tools <- coro::generator(function(turn, echo = "none") {
    tool_requests <- extract_tool_requests(turn)

    for (request in tool_requests) {
      maybe_echo_tool(request, echo = echo)
      result <- invoke_tool(request)

      if (promises::is.promise(result@value)) {
        cli::cli_abort(c(
          "Can't use async tools with `$chat()` or `$stream()`.",
          i = "Async tools are supported, but you must use `$chat_async()` or `$stream_async()`."
        ))
      }

      maybe_echo_tool(result, echo = echo)
      yield(result)
    }
  })

  # invoke_tools_async is intentionally *not* an _async_ generator, instead it
  # is a generator that returns promises. This lets the caller decide if the
  # tasks should be run in parallel or sequentially.
  invoke_tools_async <- coro::generator(function(turn, tools, echo = "none") {
    tool_requests <- extract_tool_requests(turn)

    invoke_tool_async_wrapper <- coro::async(function(request) {
      maybe_echo_tool(request, echo = echo)
      result <- coro::await(invoke_tool_async(request))
      maybe_echo_tool(result, echo = echo)
      result
    })

    for (request in tool_requests) {
      yield(invoke_tool_async_wrapper(request))
    }
  })
})

gen_async_promise_all <- function(generator) {
  promises::promise_all(.list = coro::collect(generator))
}

extract_tool_requests <- function(turn) {
  if (is.null(turn)) return(NULL)

  is_tool_request <- map_lgl(turn@contents, S7_inherits, ContentToolRequest)
  turn@contents[is_tool_request]
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

  args <- tool_request_args(request)
  if (S7_inherits(args, ContentToolResult)) {
    # Failed to convert the arguments
    return(args)
  }

  tryCatch(
    {
      result <- do.call(request@tool@fun, args)
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

    args <- tool_request_args(request)
    if (S7_inherits(args, ContentToolResult)) {
      # Failed to convert the arguments
      return(args)
    }

    tryCatch(
      {
        result <- await(do.call(request@tool@fun, args))
        new_tool_result(request, result)
      },
      error = function(e) {
        # TODO: We need to report this somehow; it's way too hidden from the user
        new_tool_result(request, error = e)
      }
    )
  })
)

tool_request_args <- function(request) {
  tool <- request@tool
  args <- request@arguments

  if (!tool@convert) {
    return(args)
  }

  extra_args <- setdiff(names(args), names(tool@arguments@properties))
  if (length(extra_args) > 0) {
    e <- catch_cnd(cli::cli_abort("Unused argument{?s}: {extra_args}"))
    return(new_tool_result(request, error = e))
  }

  convert_from_type(args, tool@arguments)
}

tool_results_as_turn <- function(results) {
  if (length(results) == 0) {
    return(NULL)
  }
  is_tool_result <- map_lgl(results, S7_inherits, ContentToolResult)
  if (!any(is_tool_result)) {
    return(NULL)
  }
  Turn("user", contents = results[is_tool_result])
}

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
  if (length(tool_errors) == 0) {
    return()
  }

  errs <- map_chr(
    tool_errors[seq_len(min(3, length(tool_errors)))],
    function(result) {
      name <- result@request@name %||% "unknown_tool"
      id <- result@request@id
      error <- tool_error_string(result)
      cli::format_inline("[{.field {name}} ({id})]: {cli_escape(error)}")
    }
  )

  cli::cli_warn(c(
    "Failed to evaluate {length(tool_errors)} tool call{?s}.",
    set_names(errs, "x"),
    "i" = if (length(errs) < length(tool_errors)) {
      cli::format_inline(
        "{cli::symbol$ellipsis} and {length(tool_errors) - length(errs)} more."
      )
    }
  ))
}

maybe_echo_tool <- function(x, echo = "output") {
  if (!identical(echo, "output")) {
    return(invisible(x))
  }

  if (S7_inherits(x, ContentToolRequest)) {
    cli::cli_text(
      cli::col_blue(cli::symbol$circle),
      " [{cli::col_blue('tool call')}] ",
      cli_escape(format(x, show = "call"))
    )
    return(invisible(x))
  }

  if (!S7_inherits(x, ContentToolResult)) {
    # neither tool result or request
    return(invisible(x))
  }

  # ContentToolResult ----
  if (tool_errored(x)) {
    icon <- cli::col_red(cli::symbol$stop)
    header <- cli::col_red("Error: ")
    value <- tool_error_string(x)
  } else {
    icon <- cli::col_green(cli::symbol$record)
    header <- ""
    value <- tool_string(x)
  }

  value <- cli::style_italic(value)

  if (grepl("\n", value)) {
    lines <- strsplit(value, "\n")[[1]]
    lines <- c(
      lines[seq_len(min(5, length(lines)))],
      if (length(lines) > 5) cli::symbol$ellipsis
    )
    lines <- cli::style_italic(lines)
    cli::cli_text("{icon} #> {header}{lines[1]}")
    for (line in lines[-1]) {
      cli::cli_text("\u00a0\u00a0#> {line}")
    }
  } else {
    max_width <- cli::console_width() - 7
    if (nchar(value) > max_width) {
      value <- substring(value, 1, max_width)
      value <- paste0(value, cli::symbol$ellipsis)
    }
    value <- cli::style_italic(value)
    cli::cli_text("{icon} #> {header}{value}")
  }

  invisible(x)
}
