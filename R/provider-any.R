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

  chat_fun(
    model = model,
    ...,
    system_prompt = system_prompt,
    params = params,
    echo = echo
  )
}
