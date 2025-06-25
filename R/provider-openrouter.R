#' @include provider-openai.R
NULL

#' Chat with one of the many models hosted on OpenRouter
#'
#' @description
#' Sign up at <https://openrouter.ai>.
#'
#' Support for features depends on the underlying model that you use; see
#' <https://openrouter.ai/models> for details.
#'
#' @export
#' @family chatbots
#' @param api_key `r api_key_param("OPENROUTER_API_KEY")`
#' @param model `r param_model("gpt-4o")`
#' @inheritParams chat_openai
#' @inherit chat_openai return
#' @examples
#' \dontrun{
#' chat <- chat_openrouter()
#' chat$chat("Tell me three jokes about statisticians")
#' }
chat_openrouter <- function(
  system_prompt = NULL,
  api_key = openrouter_key(),
  model = NULL,
  seed = NULL,
  api_args = list(),
  echo = c("none", "output", "all")
) {
  model <- set_default(model, "gpt-4o")
  echo <- check_echo(echo)

  provider <- ProviderOpenRouter(
    name = "OpenRouter",
    base_url = "https://openrouter.ai/api/v1",
    model = model,
    seed = seed,
    extra_args = api_args,
    api_key = api_key
  )
  Chat$new(provider = provider, system_prompt = system_prompt, echo = echo)
}

chat_openrouter_test <- function(..., echo = "none") {
  chat_openrouter(..., model = "openai/gpt-4o-mini-2024-07-18", echo = echo)
}

ProviderOpenRouter <- new_class(
  "ProviderOpenRouter",
  parent = ProviderOpenAI,
)

openrouter_key <- function() {
  key_get("OPENROUTER_API_KEY")
}

method(base_request, ProviderOpenRouter) <- function(provider) {
  req <- base_request(super(provider, ProviderOpenAI))
  # https://openrouter.ai/docs/api-keys
  req <- req_headers(
    req,
    `HTTP-Referer` = "https://ellmer.tidyverse.org",
    `X-Title` = "ellmer"
  )

  req
}

method(value_turn, ProviderOpenRouter) <- function(
  provider,
  result,
  has_type = FALSE
) {
  # https://openrouter.ai/docs/errors
  check_openrouter_error(result$error)

  value_turn(
    super(provider, ProviderOpenAI),
    result = result,
    has_type = has_type
  )
}

method(stream_parse, ProviderOpenRouter) <- function(provider, event) {
  if (is.null(event) || identical(event$data, "[DONE]")) {
    return(NULL)
  }

  result <- jsonlite::parse_json(event$data)
  check_openrouter_error(result$error)
  result
}

check_openrouter_error <- function(error, call = caller_env()) {
  if (is.null(error)) {
    return()
  }
  message <- error$message
  if (is.null(error$metadata$raw$data)) {
    details <- NULL
  } else {
    details <- prettify(error$metadata$raw$data)
    # don't line wrap
    details <- gsub(" ", "\u00a0", details, fixed = TRUE)
  }

  abort(
    c("message", i = if (!is.null(details)) details),
    call = call
  )
}

method(chat_resp_stream, ProviderOpenRouter) <- function(provider, resp) {
  repeat {
    event <- resp_stream_sse(resp)
    if (is.null(event)) {
      break
    }

    # https://openrouter.ai/docs/responses#sse-streaming-comments
    if (!identical(event$data, character())) {
      break
    }
    Sys.sleep(0.1)
  }

  event
}

method(as_json, list(ProviderOpenRouter, ContentText)) <- function(
  provider,
  x
) {
  if (identical(x@text, "")) {
    # Tool call requests can include a Content with empty text,
    # but it doesn't like it if you send this back
    NULL
  } else {
    list(type = "text", text = x@text)
  }
}
