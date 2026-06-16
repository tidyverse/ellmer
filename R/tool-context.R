#' @include ellmer-package.R
NULL

#' Access the current tool context
#'
#' @description
#' When an ellmer tool is called by an LLM, `tool_context()` returns a
#' context object with three fields:
#'
#' - `$request` — the [ContentToolRequest] for this call, with sub-fields
#'   `@name`, `@id`, `@arguments`, and `@tool`.
#' - `$store` — the chat's shared state environment (`chat$store`). This is
#'   the **same** environment as `chat$store` (by reference), so mutations
#'   made inside a tool are immediately visible to subsequent tool calls and
#'   to the `Chat` object. Use it for counters, database connections, loggers,
#'   and any other per-conversation state you don't want to expose to the
#'   model.
#' - `$turns` — an eager snapshot of the conversation history (list of [Turn]
#'   objects), including the system prompt (if any) as the first turn, up to
#'   and including the assistant turn that issued this tool request. Sibling
#'   tool results from the same turn are not included (they are appended after
#'   the current tool loop finishes).
#'
#' `tool_context()` aborts with class
#' `ellmer_error_tool_context_unavailable` if the stack is empty, which
#' happens in two situations:
#'
#' 1. The function was called outside any tool invocation (e.g. in a test or
#'    top-level script without a live chat).
#' 2. The function was called after an `await()` in an async tool. The
#'    context frame closes at the first `await`, so you must capture the
#'    context before yielding: `ctx <- tool_context()`.
#'
#' @section Async tools:
#' In async (coro-based) tools the `with_tool_context()` frame is closed at
#' the synchronous-prefix boundary — that is, when the tool's `do.call`
#' returns its promise, before any `await()` resolves. Capture the context
#' at the top of the tool body before any `await()` call:
#'
#' ```r
#' my_tool <- tool(
#'   coro::async(function() {
#'     ctx <- tool_context()         # capture BEFORE await
#'     result <- await(some_promise())
#'     ctx$store$n <- ctx$store$n + 1L  # safe: $store is by reference
#'     result
#'   }),
#'   description = "An async tool that updates the store"
#' )
#' ```
#'
#' @return `tool_context()` returns the current `ellmer_tool_context` object
#'   (a classed list with fields `$request`, `$store`, `$turns`).
#'
#'   `with_tool_context()` returns the value of `code`.
#'
#'   `local_tool_context()` returns `context` invisibly.
#'
#' @param context An `ellmer_tool_context` object, or a list with fields
#'   `request`, `store`, and `turns` (which will be promoted automatically).
#' @param code An expression to evaluate with `context` on top of the stack.
#' @param .frame The environment whose exit triggers the pop. Defaults to
#'   `parent.frame()` (the calling function's frame).
#'
#' @examples
#' # Increment a counter stored in chat$store
#' counter_tool <- tool(
#'   function() {
#'     ctx <- tool_context()
#'     ctx$store$n <- (ctx$store$n %||% 0L) + 1L
#'     ctx$store$n
#'   },
#'   description = "Increment and return the call counter"
#' )
#'
#' # Test a tool that uses tool_context() without a live chat
#' test_store <- new.env(parent = emptyenv())
#' test_store$n <- 0L
#' local_tool_context(list(request = NULL, store = test_store, turns = list()))
#' # now tool_context() returns the context inside this frame
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
  on.exit(pop_tool_context(), add = TRUE)
  force(code)
}

#' @rdname tool_context
#' @export
local_tool_context <- function(context, .frame = parent.frame()) {
  push_tool_context(context)
  withr::defer(pop_tool_context(), envir = .frame)
  invisible(context)
}

new_tool_context <- function(request, store, turns) {
  structure(
    list(request = request, store = store, turns = turns),
    class = "ellmer_tool_context"
  )
}

push_tool_context <- function(context) {
  context <- as_tool_context(context)
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
    store = context[["store"]] %||% new.env(parent = emptyenv()),
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

tool_context_factory <- function(store, turns) {
  function(request) new_tool_context(request, store, turns)
}
