# Results a content list
invoke_tools <- function(turn, tools) {
  if (length(tools) == 0) {
    return()
  }
  tool_requests <- extract_tool_requests(turn@contents)

  lapply(tool_requests, function(call) {
    result <- invoke_tool(tools[[call@name]], call@arguments, call@id)

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
    if (length(tools) == 0) {
      return()
    }

    tool_requests <- extract_tool_requests(turn@contents)

    # We call it this way instead of a more natural for + await_each() because
    # we want to run all the async tool calls in parallel
    result_promises <- lapply(tool_requests, function(call) {
      invoke_tool_async(tools[[call@name]], call@arguments, call@id)
    })

    promises::promise_all(.list = result_promises)
  })
)

extract_tool_requests <- function(contents) {
  is_tool_request <- map_lgl(contents, S7_inherits, ContentToolRequest)
  contents[is_tool_request]
}

tool_result_factory <- function(tool, arguments, id) {
  function(result = NULL, error = NULL) {
    if (S7_inherits(result, ContentToolResult)) {
      result@id <- id
      result@call_tool <- tool
      result@call_args <- arguments
      return(result)
    }

    ContentToolResult(
      value = result,
      error = error,
      id = id,
      call_tool = tool,
      call_args = arguments
    )
  }
}

# Also need to handle edge cases: https://platform.openai.com/docs/guides/function-calling/edge-cases
invoke_tool <- function(tool, arguments, id) {
  tool_result <- tool_result_factory(tool, arguments, id)

  if (is.null(tool) || is.null(tool@fun)) {
    return(tool_result(error = "Unknown tool"))
  }

  fun <- tool@fun

  tryCatch(
    tool_result(do.call(fun, arguments)),
    error = function(e) {
      # TODO: We need to report this somehow; it's way too hidden from the user
      tool_result(error = e)
    }
  )
}

on_load(
  invoke_tool_async <- coro::async(function(tool, arguments, id) {
    tool_result <- tool_result_factory(tool, arguments, id)

    if (is.null(tool) || is.null(tool@fun)) {
      return(tool_result(error = "Unknown tool"))
    }

    fun <- tool@fun

    tryCatch(
      {
        result <- await(do.call(fun, arguments))
        tool_result(result)
      },
      error = function(e) {
        # TODO: We need to report this somehow; it's way too hidden from the user
        tool_result(error = e)
      }
    )
  })
)
