# Results a content list
invoke_tools <- function(turn) {
  tool_requests <- extract_tool_requests(turn@contents)

  lapply(tool_requests, function(request) {
    result <- invoke_tool(request)

    if (promises::is.promise(result@value)) {
      cli::cli_abort(c(
        "Can't use async tools with `$chat()` or `$stream()`.",
        i = "Async tools are supported, but you must use `$chat_async()` or `$stream_async()`."
      ))
    }

    result
  })
}

on_load(
  invoke_tools_async <- coro::async(function(turn, tools) {
    tool_requests <- extract_tool_requests(turn@contents)

    # We call it this way instead of a more natural for + await_each() because
    # we want to run all the async tool calls in parallel
    result_promises <- lapply(tool_requests, function(request) {
      invoke_tool_async(request)
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
