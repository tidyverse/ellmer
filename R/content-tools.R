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

as_content_tool_result_factory <- function(request) {
  force(request)

  function(result = NULL, error = NULL) {
    if (S7_inherits(result, ContentToolResult)) {
      result@id <- request@id
      result@request <- request
      return(result)
    }

    ContentToolResult(
      value = result,
      error = error,
      id = request@id,
      request = request
    )
  }
}

# Also need to handle edge cases: https://platform.openai.com/docs/guides/function-calling/edge-cases
invoke_tool <- function(request) {
  as_content_tool_result <- as_content_tool_result_factory(request)

  if (is.null(request@tool)) {
    return(as_content_tool_result(error = "Unknown tool"))
  }

  tryCatch(
    as_content_tool_result(do.call(request@tool@fun, request@arguments)),
    error = function(e) {
      # TODO: We need to report this somehow; it's way too hidden from the user
      as_content_tool_result(error = e)
    }
  )
}

on_load(
  invoke_tool_async <- coro::async(function(request) {
    as_content_tool_result <- as_content_tool_result_factory(request)

    if (is.null(request@tool)) {
      return(as_content_tool_result(error = "Unknown tool"))
    }

    tryCatch(
      {
        result <- await(do.call(request@tool@fun, request@arguments))
        as_content_tool_result(result)
      },
      error = function(e) {
        # TODO: We need to report this somehow; it's way too hidden from the user
        as_content_tool_result(error = e)
      }
    )
  })
)
