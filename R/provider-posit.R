#' @include provider-claude.R
#' @include provider-openai-compatible.R
NULL

#' Chat with a model hosted by Posit AI
#'
#' @description
#' `r support_badge("official")`
#'
#' [Posit AI](https://posit.ai) provides access to a curated set of models
#' for Posit subscribers.
#'
#' ## Authentication
#'
#' By default, `chat_posit()` authenticates with an OAuth device flow against
#' `login.posit.cloud`: the first time you use it, you'll be prompted to
#' visit a URL and enter a code. The resulting tokens are cached on disk
#' (see [httr2::req_oauth_device()]) and refreshed automatically, so you
#' should only need to do this once per machine.
#'
#' @param base_url The base URL of the Posit AI gateway.
#' @param credentials A zero-argument function that returns the
#'   credentials to use in place of the default OAuth device flow, either as a
#'   named list of headers or as a function that modifies the request. You
#'   should not usually need to set this.
#' @param model `r param_model("claude-sonnet-4-6", "posit")`
#' @param cache How long to cache inputs? Defaults to "5m" (five minutes).
#'   Set to "none" to disable caching or "1h" to cache for one hour. This is
#'   only supported for Claude models and is ignored for other models.
#' @inheritParams chat_openai
#' @inheritParams chat_anthropic
#' @inherit chat_openai return
#' @family chatbots
#' @export
#' @examples
#' \dontrun{
#' chat <- chat_posit()
#' chat$chat("Tell me three jokes about statisticians")
#' }
chat_posit <- function(
  system_prompt = NULL,
  base_url = "https://gateway.posit.ai",
  credentials = NULL,
  model = NULL,
  params = NULL,
  cache = c("5m", "1h", "none"),
  api_args = list(),
  api_headers = character(),
  echo = NULL
) {
  check_string(base_url)
  model <- set_default(model, "claude-sonnet-4-6")
  cache <- arg_match(cache)
  echo <- check_echo(echo)

  credentials <- as_credentials(
    "chat_posit",
    default_posit_credentials(),
    credentials = credentials
  )

  if (is_claude_model(model)) {
    provider <- ProviderPositAnthropic(
      name = "Posit",
      base_url = paste0(base_url, "/anthropic/v1"),
      model = model,
      params = params %||% params(),
      extra_args = api_args,
      extra_headers = api_headers,
      credentials = credentials,
      cache = cache
    )
  } else {
    provider <- ProviderPositOpenAI(
      name = "Posit",
      base_url = paste0(base_url, "/openai/v1"),
      model = model,
      params = params %||% params(),
      extra_args = api_args,
      extra_headers = api_headers,
      credentials = credentials
    )
  }

  Chat$new(provider = provider, system_prompt = system_prompt, echo = echo)
}

#' @export
#' @rdname chat_posit
models_posit <- function(
  base_url = "https://gateway.posit.ai",
  credentials = NULL
) {
  check_string(base_url)

  credentials <- as_credentials(
    "models_posit",
    default_posit_credentials(),
    credentials = credentials
  )

  req <- request(base_url)
  req <- req_url_path_append(req, "/models")
  req <- ellmer_req_credentials(req, credentials(), "Authorization")
  req <- ellmer_req_user_agent(req)
  req <- req_error(req, body = posit_error_body)
  resp <- req_perform(req)
  json <- resp_body_json(resp)

  data.frame(
    id = map_chr(json$chat, "[[", "id"),
    name = map_chr(json$chat, "[[", "display_name")
  )
}

ProviderPositAnthropic <- new_class(
  "ProviderPositAnthropic",
  parent = ProviderAnthropic
)

ProviderPositOpenAI <- new_class(
  "ProviderPositOpenAI",
  parent = ProviderOpenAICompatible
)

method(base_request, ProviderPositAnthropic) <- function(provider) {
  req <- base_request(super(provider, ProviderAnthropic))
  req_error(req, body = posit_error_body)
}

method(base_request, ProviderPositOpenAI) <- function(provider) {
  req <- base_request(super(provider, ProviderOpenAICompatible))
  req_error(req, body = posit_error_body)
}

method(models_list, ProviderPositAnthropic) <- function(provider) {
  models_posit(
    base_url = posit_gateway_url(provider@base_url),
    credentials = provider@credentials
  )
}

method(models_list, ProviderPositOpenAI) <- function(provider) {
  models_posit(
    base_url = posit_gateway_url(provider@base_url),
    credentials = provider@credentials
  )
}

is_claude_model <- function(model) {
  grepl("^claude", model)
}

posit_gateway_url <- function(base_url) {
  sub("/(anthropic|openai)/v1/?$", "", base_url)
}

default_posit_credentials <- function() {
  client <- posit_oauth_client()
  function() {
    function(req) {
      req_oauth_device(
        req,
        client = client,
        auth_url = "https://login.posit.cloud/oauth/device/authorize",
        scope = "prism",
        # posit.cloud only mints a gateway-authorized token when `scope` is
        # also sent on the token exchange, not just the authorize request.
        token_params = list(scope = "prism"),
        cache_disk = TRUE
      )
    }
  }
}

posit_oauth_client <- function() {
  oauth_client(
    id = "rstudio-ide",
    token_url = "https://login.posit.cloud/oauth/token",
    name = "ellmer-posit"
  )
}

posit_oauth_reset <- function() {
  httr2::oauth_cache_clear(posit_oauth_client(), cache_disk = TRUE)
}

posit_error_body <- function(resp) {
  json <- tryCatch(
    resp_body_json(resp, check_type = FALSE),
    error = function(cnd) NULL
  )

  # The gateway's own message here leaks an internal user ID and gives no
  # actionable next step, so substitute a clearer one.
  if (identical(json$error_type, "prism_account_not_found")) {
    return(c(
      "You must finish setting up your Posit AI account before using the API.",
      i = "Visit <https://posit.ai/> to accept the service agreement."
    ))
  }

  # The gateway reports its own errors with a string `error`, but passes
  # upstream Anthropic/OpenAI errors through verbatim, where it's an object.
  error <- json$error
  if (is_string(error)) {
    error
  } else if (is.list(error)) {
    error$message
  } else {
    prettify(resp_body_string(resp))
  }
}
