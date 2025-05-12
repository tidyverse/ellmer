#' Callback Manager
#'
#' A simple class to manage a collection of callback functions that can be
#' invoked sequentially with a single call to `$invoke()` with an object or data
#' to pass to the callback functions. Callbacks must take at least one argument
#' are invoked in reverse order of their registration.
#'
#' @noRd
CallbackManager <- R6Class(
  "CallbackManager",

  public = list(
    initialize = function(args = NULL) {
      private$args <- args
    },

    #' @description Add a callback function.
    #' @param callback A function to be called.
    #' @return A function that can be called to remove the callback.
    add = function(callback, call = caller_env()) {
      check_function2(callback, args = private$args, call = call)

      id <- private$next_id()
      private$callbacks[[id]] <- callback

      fn_remove <- function() {
        private$callbacks[[id]] <- NULL
        invisible()
      }
      invisible(fn_remove)
    },

    #' @description
    #' Invoke all registered callbacks with the provided arguments. Callbacks
    #' are invoked in reverse order of registration (last-in first-evaluated).
    #'
    #' @param ... Arguments to pass to the callbacks.
    #' @returns Nothing, callbacks are invoked for side effects).
    invoke = function(...) {
      if (length(private$callbacks) == 0) {
        return(invisible(NULL))
      }

      # Invoke callbacks in reverse insertion order
      for (id in rev(as.integer(names(private$callbacks)))) {
        res <- private$callbacks[[as.character(id)]](...)
        if (promises::is.promise(res)) {
          cli::cli_abort(c(
            "Can't use async callbacks with `$chat()` or `$stream()`.",
            i = "Async callbacks are supported, but you must use `$chat_async()` or `$stream_async()`."
          ))
        }
      }

      invisible(NULL)
    },

    #' @description
    #' Invoke all registered callbacks asynchronously with the provided
    #' arguments. As with `$invoke()`, callbacks are invoked in reverse order of
    #' registration (last-in first-evaluated).
    #'
    #' @param ... Arguments to pass to the callbacks.
    #' @returns Nothing, callbacks are invoked for side effects).
    invoke_async = async_method(function(self, private, ...) {
      if (length(private$callbacks) == 0) {
        return(invisible(NULL))
      }

      # Invoke callbacks in reverse insertion order
      for (id in rev(as.integer(names(private$callbacks)))) {
        coro::await(exec(private$callbacks[[as.character(id)]], ...))
      }

      invisible(NULL)
    }),

    #' @description Get the number of registered callbacks.
    #' @return Integer count of callbacks.
    count = function() {
      length(private$callbacks)
    },

    #' @description Clear all registered callbacks.
    clear = function() {
      private$callbacks <- list()
      invisible(NULL)
    },

    #' @describeIn Get callback list
    get_callbacks = function() {
      private$callbacks
    }
  ),

  private = list(
    callbacks = list(),
    args = NULL,

    id = 1L,
    next_id = function() {
      id <- private$id
      private$id <- private$id + 1L
      as.character(id)
    }
  )
)

# From https://github.com/r-lib/httr2/blob/da2724ae/R/utils.R#L179-L235
check_function2 <- function(
  x,
  ...,
  args = NULL,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  check_function(
    x = x,
    allow_null = allow_null,
    arg = arg,
    call = call
  )

  if (!is.null(x)) {
    .check_function_args(
      f = x,
      expected_args = args,
      arg = arg,
      call = call
    )
  }
}

.check_function_args <- function(f, expected_args, arg, call) {
  if (is_null(expected_args)) {
    return(invisible(NULL))
  }

  actual_args <- fn_fmls_names(f) %||% character()
  missing_args <- setdiff(expected_args, actual_args)
  if (is_empty(missing_args)) {
    return(invisible(NULL))
  }

  n_expected_args <- length(expected_args)
  n_actual_args <- length(actual_args)

  if (n_actual_args == 0) {
    arg_info <- "instead it has no arguments"
  } else {
    arg_info <- paste0("it currently has {.arg {actual_args}}")
  }

  cli::cli_abort(
    paste0(
      "{.arg {arg}} must have the {cli::qty(n_expected_args)}argument{?s} {.arg {expected_args}}; ",
      arg_info,
      "."
    ),
    call = call,
    arg = arg
  )
}
