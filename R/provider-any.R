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
  } else {
    provider <- pieces[[1]]
    model <- paste(pieces[-1], collapse = "/")
  }

  provider_fn_name <- paste0("chat_", provider)
  chat_fun <- env_get(asNamespace("ellmer"), provider_fn_name, default = NULL)
  if (is.null(chat_fun)) {
    cli::cli_abort("Can't find provider {.code ellmer::{provider_fn_name}()}.")
  }

  required_params <- c("model", "system_prompt", "params")
  if (any(!required_params %in% fn_fmls_names(chat_fun))) {
    cli::cli_abort(
      "{.fn ellmer::chat} does not support {.fn ellmer::{provider_fn_name}}, please call it directly.",
    )
  }

  chat_fun(
    model = model,
    ...,
    system_prompt = system_prompt,
    params = params,
    echo = echo
  )
}
