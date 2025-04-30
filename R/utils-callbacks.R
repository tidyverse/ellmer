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
    #' @description Add a callback function.
    #' @param callback A function to be called.
    #' @return A function that can be called to remove the callback.
    add = function(callback) {
      if (!is.function(callback)) {
        cli::cli_abort("{.var callback} must be a function.")
      }

      arg_names <- fn_fmls_names(callback)

      if (length(arg_names) == 0) {
        cli::cli_abort(
          "{.var callback} must accept at least one argument or '...'."
        )
      }

      # Can't have more than one required arg and required arg must be first
      is_required <- map2_lgl(
        arg_names,
        fn_fmls(callback),
        function(arg, val) {
          arg != "..." && is.symbol(val) && identical(val, quote(expr = ))
        }
      )

      ok_args <-
        sum(is_required) == 0 ||
        (sum(is_required) == 1 && is_required[1])

      if (!ok_args) {
        cli::cli_abort(
          "Only the first argument of {.var callback} can be required."
        )
      }

      id <- private$next_id()
      private$callbacks[[as.character(id)]] <- callback

      function() {
        private$callbacks[[as.character(id)]] <- NULL
        invisible(NULL)
      }
    },

    #' @description
    #' Invoke all registered callbacks with the provided arguments. Callbacks
    #' are invoked in reverse order of registration (last-in first-evaluated).
    #'
    #' @param data An input, such as a list or object, to pass to the callbacks.
    #' @returns Nothing, callbacks are invoked for side effects).
    invoke = function(data) {
      if (length(private$callbacks) == 0) {
        return(invisible(NULL))
      }

      # Invoke callbacks in reverse insertion order
      for (id in rev(as.integer(names(private$callbacks)))) {
        exec(private$callbacks[[as.character(id)]], data)
      }

      invisible(NULL)
    },

    #' @description Get the number of registered callbacks.
    #' @return Integer count of callbacks.
    count = function() {
      length(private$callbacks)
    },

    #' @description Clear all registered callbacks.
    clear = function() {
      private$callbacks <- list()
      invisible(NULL)
    }
  ),

  private = list(
    callbacks = list(),
    id = 1L,

    next_id = function() {
      id <- private$id
      private$id <- private$id + 1L
      id
    }
  )
)
