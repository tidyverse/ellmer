#' @include turns.R
NULL

maybe_invoke_callbacks_tool_request <- function(callbacks, request) {
  cb <- callbacks$tool_request
  if (is.null(cb)) return()

  tryCatch(
    {
      cb$invoke(request)
      NULL
    },
    ellmer_tool_reject = function(e) {
      ContentToolResult(error = e$message, request = request)
    }
  )
}

maybe_invoke_callbacks_tool_request_async <- coro::async(
  function(callbacks, request) {
    cb <- callbacks$tool_request
    if (is.null(cb)) return()

    tryCatch(
      {
        coro::await(cb$invoke_async(request))
        NULL
      },
      ellmer_tool_reject = function(e) {
        ContentToolResult(error = e$message, request = request)
      }
    )
  }
)

maybe_invoke_callbacks_tool_result <- function(callbacks, result) {
  cb <- callbacks$tool_result
  if (is.null(cb)) return()
  cb$invoke(result)
}

# Results a content list
invoke_tools <- function(turn, echo = "none", callbacks = list()) {
  tool_requests <- extract_tool_requests(turn@contents)

  lapply(tool_requests, function(request) {
    maybe_echo_tool(request, echo = echo)
    rejected <- maybe_invoke_callbacks_tool_request(callbacks, request)
    if (!is.null(rejected)) {
      maybe_echo_tool(rejected, echo = echo)
      return(rejected)
    }

    result <- invoke_tool(request)

    if (promises::is.promise(result@value)) {
      cli::cli_abort(c(
        "Can't use async tools with `$chat()` or `$stream()`.",
        i = "Async tools are supported, but you must use `$chat_async()` or `$stream_async()`."
      ))
    }

    maybe_echo_tool(result, echo = echo)
    maybe_invoke_callbacks_tool_result(callbacks, result)
    result
  })
}

on_load(
  invoke_tools_async <- coro::async(function(
    turn,
    tools,
    echo = "none",
    callbacks = list()
  ) {
    tool_requests <- extract_tool_requests(turn@contents)

    invoke_tool_async_wrapper <- coro::async(function(request) {
      maybe_echo_tool(request, echo = echo)
      rejected <- coro::await(
        maybe_invoke_callbacks_tool_request_async(callbacks, request)
      )
      if (!is.null(rejected)) {
        maybe_echo_tool(rejected, echo = echo)
        return(rejected)
      }

      result <- coro::await(invoke_tool_async(request))
      maybe_echo_tool(result, echo = echo)
      maybe_invoke_callbacks_tool_result(callbacks, result)
      result
    })

    run_in_parallel <- getOption("ellmer.tool_async_parallel", TRUE)

    if (isTRUE(run_in_parallel)) {
      result_promises <- lapply(
        tool_requests,
        invoke_tool_async_wrapper
      )
      promises::promise_all(.list = result_promises)
    } else {
      gen_tool_results <- coro::async_generator(function(tool_requests) {
        for (request in tool_requests) {
          result <- coro::await(invoke_tool_async_wrapper(request))
          yield(result)
        }
      })
      coro::async_collect(gen_tool_results(tool_requests))
    }
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
