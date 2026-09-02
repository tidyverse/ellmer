#' @include ellmer-package.R
NULL

#' Access the current tool context
#'
#' @description
#' When an ellmer tool is called by an LLM, `tool_context()` returns a context
#' object with two fields:
#'
#' - `$request`: the [ContentToolRequest] for this call, with sub-fields
#'   `@name`, `@id`, `@arguments`, and `@tool`.
#' - `$turns`: an eager snapshot of the conversation history (list of [Turn]
#'   objects), including the system prompt (if any) as the first turn, up to and
#'   including the assistant turn that issued this tool request. Sibling tool
#'   results from the same turn are not included (they are appended after the
#'   current tool loop finishes).
#'
#' `tool_context()` aborts with class `ellmer_error_tool_context_unavailable` if
#' the stack is empty, which happens in two situations:
#'
#' 1. The function was called outside any tool invocation (e.g. in a test or
#'    top-level script without a live chat).
#' 2. The function was called after an `await()` in an async tool. The context
#'    frame closes at the first `await`, so you must capture the context before
#'    yielding: `ctx <- tool_context()`.
#'
#' The `with_tool_context()` and `local_tool_context()` helpers are useful for
#' testing a tool function that calls `tool_context()` outside a live chat. They
#' temporarily make a supplied context available while test code runs.
#'
#' @return `tool_context()` returns the current `ellmer_tool_context` object (a
#'   classed list with fields `$request`, `$turns`).
#'
#'   `with_tool_context()` returns the value of `code`.
#'
#'   `local_tool_context()` returns `context` invisibly.
#'
#' @param context An `ellmer_tool_context` object, or a list with fields
#'   `request` and `turns` (which will be promoted automatically).
#' @param code An expression to evaluate with `context` on top of the stack.
#' @param .frame The environment whose exit triggers the pop. Defaults to
#'   `parent.frame()` (the calling function's frame).
#'
#' @examples
#' # Log the id of the request that triggered this tool call
#' logging_tool <- tool(
#'   function() {
#'     ctx <- tool_context()
#'     message("Handled request ", ctx$request@id)
#'     "done"
#'   },
#'   name = "logging_tool",
#'   description = "Log the current request id and return"
#' )
#'
#' # Test a tool that uses tool_context() without a live chat
#' request <- ContentToolRequest(
#'   id = "test-request",
#'   name = "logging_tool",
#'   arguments = list(),
#'   tool = logging_tool
#' )
#' with_tool_context(
#'   list(request = request, turns = list()),
#'   logging_tool()
#' )
#'
#' @rdname tool_context
#' @export
tool_context <- function() {
  stack <- the$tool_context_stack
  if (length(stack) == 0L) {
    cli::cli_abort(
      c(
        "No tool context is available.",
        "i" = paste0(
          "{.fn tool_context} must be called from inside an active tool ",
          "invocation. If you are writing an async tool, capture the context ",
          "before any {.fn await} call: {.code ctx <- tool_context()}."
        )
      ),
      class = "ellmer_error_tool_context_unavailable"
    )
  }
  stack[[length(stack)]]
}

#' @rdname tool_context
#' @export
with_tool_context <- function(context, code) {
  push_tool_context(context)
  withr::defer(pop_tool_context())
  force(code)
}

#' @rdname tool_context
#' @export
local_tool_context <- function(context, .frame = parent.frame()) {
  push_tool_context(context)
  withr::defer(pop_tool_context(), envir = .frame)
  invisible(context)
}

new_tool_context <- function(request, turns = list()) {
  structure(
    list(request = request, turns = turns),
    class = "ellmer_tool_context"
  )
}

push_tool_context <- function(context, call = caller_env()) {
  context <- as_tool_context(context, call = call)
  the$tool_context_stack <- c(the$tool_context_stack, list(context))
  invisible(NULL)
}

as_tool_context <- function(context, call = caller_env()) {
  if (inherits(context, "ellmer_tool_context")) {
    return(context)
  }
  if (!is.list(context)) {
    cli::cli_abort(
      "{.arg context} must be an {.cls ellmer_tool_context} object or a list.",
      call = call
    )
  }
  new_tool_context(
    request = context[["request"]],
    turns = context[["turns"]] %||% list()
  )
}

pop_tool_context <- function() {
  stack <- the$tool_context_stack
  if (length(stack) > 0L) {
    the$tool_context_stack <- stack[-length(stack)]
  }
  invisible(NULL)
}
