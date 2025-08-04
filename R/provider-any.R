#' Chat with any provider
#'
#' This is a generic interface to all the other `chat_` functions that allow
#' to you pick the provider and the model with a simple string.
#'
#' @inheritParams chat_openai
#' @param name Provider (and optionally model) name in the form
#'   `"provider/model"` or `"provider"` (which will use the default model
#'   for that provider).
#' @param ... Arguments passed to the provider function.
#' @rdname chat-any
#' @export
chat <- function(
  name,
  ...,
  system_prompt = NULL,
  params = NULL,
  echo = c("none", "output", "all")
) {
  check_string(name, allow_empty = FALSE)
  pieces <- strsplit(name, "/", fixed = TRUE)[[1]]

  if (length(pieces) == 1) {
    provider <- pieces[[1]]
    model <- NULL
  } else if (length(pieces) == 2) {
    provider <- pieces[[1]]
    model <- pieces[[2]]
  } else {
    cli::cli_abort(
      "{.arg name} must be in form {.str provider} or {.str provider/model}."
    )
  }

  provider_name <- paste0("chat_", pieces[[1]])
  chat_fun <- env_get(asNamespace("ellmer"), provider_name, default = NULL)
  if (is.null(chat_fun)) {
    cli::cli_abort("Can't find provider {.code ellmer::{provider_name}()}.")
  }

  dots <- dots_list(..., params = params, echo = echo)

  # Drop unused arguments...
  chat_fun_fmls <- fn_fmls_names(chat_fun)
  if ("..." %in% chat_fun_fmls) {
    # If the function accepts `...`, we assume all arguments are accepted
    args_matched <- names(dots)
  } else {
    # Otherwise, match arguments against the function's formal arguments
    args_matched <- intersect(names(dots), fn_fmls_names(chat_fun))
    # with a warning for user-provided arguments that are not used
    args_ignored <- setdiff(names(dots), c("params", "echo", args_matched))
    if (length(args_ignored) > 0) {
      cli::cli_warn(
        "Ignoring {.var {args_ignored}} argument{?s} that {?is/are} not used by {.fn ellmer::{provider_name}}.",
      )
    }
  }

  # A bit overkill, but ensures the chat_*() function appears in tracebacks
  chat_call <- call2(
    provider_name,
    model = model,
    system_prompt = system_prompt,
    !!!dots[args_matched],
    .ns = "ellmer"
  )

  eval(chat_call)
}
