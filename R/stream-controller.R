#' Create a stream controller
#'
#' @description
#' Creates a controller that can cancel an in-progress stream. Pass it to
#' [Chat]'s `$stream()` or `$stream_async()` via the `controller` argument,
#' then call `$cancel()` from anywhere (e.g. a Shiny observer) to stop the
#' stream after the next chunk arrives.
#'
#' The same controller can be reused across multiple streams. Call
#' `$reset()` to clear the cancelled state, or pass it directly to a new
#' `$stream()` call — it will be reset automatically.
#'
#' @section Async cancellation in Shiny:
#'
#' In a Shiny app, use an [ExtendedTask][shiny::ExtendedTask] for
#' non-blocking chat and a `stream_controller()` to wire up a cancel
#' button:
#'
#' ```r
#' controller <- stream_controller()
#'
#' chat_task <- ExtendedTask$new(function(user_query, controller = NULL) {
#'   chat <- chat_openai(model = "gpt-4.1-nano")
#'   stream <- chat$stream_async(user_query, controller = controller)
#'   shinychat::markdown_stream("response", stream)
#' })
#'
#' observeEvent(input$ask, {
#'   controller <<- stream_controller()
#'   chat_task$invoke(input$query, controller = controller)
#' })
#'
#' observeEvent(input$cancel, {
#'   controller$cancel()
#' })
#' ```
#'
#' @return An `ellmer_stream_controller` object with the following
#'   elements:
#'
#'   * `$cancel(reason = "cancelled")`: Cancel the stream. The `reason`
#'     string is stored on the controller and used as the
#'     [AssistantPartialTurn][Turn]'s `reason` property.
#'   * `$reset()`: Clear the cancelled state and reason.
#'   * `$cancelled`: A logical flag indicating whether the controller
#'     has been cancelled.
#'   * `$reason`: The cancellation reason string, or `NULL` if not
#'     cancelled.
#'
#' @examplesIf rlang::is_interactive()
#' chat <- chat_openai(model = "gpt-5.4-nano")
#'
#' ctrl <- stream_controller()
#' stream <- chat$stream("Write a short story.", controller = ctrl)
#'
#' i <- 0
#' coro::loop(for (chunk in stream) {
#'   i <- i + 1
#'   if (i > 10) ctrl$cancel()
#' })
#'
#' chat
#'
#' @export
stream_controller <- function() {
  StreamController$new()
}

StreamController <- R6::R6Class(
  "ellmer_stream_controller",
  public = list(
    cancel = function(reason = "cancelled") {
      check_string(reason)
      private$.reason <- reason
      private$.cancelled <- TRUE
    },

    reset = function() {
      private$.cancelled <- FALSE
      private$.reason <- NULL
    },

    print = function(...) {
      status <- if (private$.cancelled) {
        sprintf('cancelled="%s"', private$.reason)
      } else {
        "active"
      }
      cat("<ellmer_stream_controller ", status, ">\n", sep = "")
      invisible(self)
    }
  ),
  active = list(
    cancelled = function(value) {
      if (!missing(value)) {
        cli::cli_abort(
          "Use the {.code $cancel()} method to cancel the stream or {.code $reset()} to reset the stream controller."
        )
      }
      private$.cancelled
    },
    reason = function(value) {
      if (!missing(value)) {
        cli::cli_abort(
          "Use the {.code $cancel()} method to cancel the stream or {.code $reset()} to reset the stream controller."
        )
      }
      private$.reason
    }
  ),
  private = list(
    .cancelled = FALSE,
    .reason = NULL
  )
)

as_controller <- function(controller, reset = TRUE, call = caller_env()) {
  if (is.null(controller)) {
    return(stream_controller())
  }

  if (!inherits(controller, "ellmer_stream_controller")) {
    cli::cli_abort(
      "{.arg controller} must be an {.cls ellmer_stream_controller} object created by {.fn stream_controller}.",
      call = call
    )
  }

  if (reset && controller$cancelled) {
    controller$reset()
  }

  controller
}
