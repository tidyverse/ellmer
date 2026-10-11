#' @include provider-openai-compatible.R
NULL

#' Chat with one of the many models hosted on OpenRouter
#'
#' @description
#' `r support_badge("community")`
#'
#' Sign up at <https://openrouter.ai>.
#'
#' Support for features depends on the underlying model that you use; see
#' <https://openrouter.ai/models> for details.
#'
#' ## Authentication
#'
#' If the `OPENROUTER_API_KEY` environment variable is set, `chat_openrouter()`
#' uses it. Otherwise, the first time you use it in an interactive session,
#' your browser will open so you can log in to OpenRouter and authorize ellmer.
#' In a hosted session, such as Posit Workbench, OpenRouter can't send you
#' back to R, so you'll be asked to paste in the code it shows you instead.
#' The resulting key is cached on disk (see [httr2::oauth_cache_path()]), so
#' you should only need to do this once per machine. You can see and revoke the
#' key at <https://openrouter.ai/keys>; if you revoke it, you'll be asked to
#' log in again the next time you use `chat_openrouter()`.
#'
#' @export
#' @family chatbots
#' @param api_key `r lifecycle::badge("deprecated")` Use `credentials` instead.
#' @param credentials A zero-argument function that returns the credentials
#'   to use in place of `OPENROUTER_API_KEY` or the default browser login,
#'   either as a string, a named list of headers, or a function that modifies
#'   the request. You should not usually need to set this.
#' @param model `r param_model("gpt-5.6-terra")`
#' @param params Common model parameters, usually created by [params()].
#' @inheritParams chat_openai
#' @inheritParams chat_openai_compatible
#' @inherit chat_openai return
#' @examples
#' \dontrun{
#' chat <- chat_openrouter()
#' chat$chat("Tell me three jokes about statisticians")
#' }
chat_openrouter <- function(
  system_prompt = NULL,
  api_key = NULL,
  credentials = NULL,
  model = NULL,
  params = NULL,
  api_args = list(),
  echo = c("none", "output", "all"),
  api_headers = character(),
  strict = FALSE
) {
  model <- set_default(model, "gpt-5.6-terra")
  echo <- check_echo(echo)

  credentials <- as_credentials(
    "chat_openrouter",
    default_openrouter_credentials(),
    credentials = credentials,
    api_key = api_key
  )

  params <- params %||% params()

  provider <- ProviderOpenRouter(
    name = "OpenRouter",
    base_url = "https://openrouter.ai/api/v1",
    credentials = credentials,
    extra_headers = api_headers,
    preserve_thinking = TRUE,
    strict = strict
  )
  model <- Model(name = model, params = params, extra_args = api_args)
  Chat$new(
    provider = provider,
    model = model,
    system_prompt = system_prompt,
    echo = echo
  )
}

chat_openrouter_test <- function(..., echo = "none") {
  chat_openrouter(..., model = "openai/gpt-4o-mini-2024-07-18", echo = echo)
}

ProviderOpenRouter <- new_class(
  "ProviderOpenRouter",
  parent = ProviderOpenAICompatible,
)

method(chat_params, ProviderOpenRouter) <- function(provider, params) {
  # https://openrouter.ai/docs/api-reference/parameters
  standardise_params(
    params,
    c(
      temperature = "temperature",
      top_p = "top_p",
      top_k = "top_k",
      frequency_penalty = "frequency_penalty",
      presence_penalty = "presence_penalty",
      seed = "seed",
      max_tokens = "max_tokens",
      logprobs = "log_probs",
      stop = "stop_sequences"
    )
  )
}

# Authentication ---------------------------------------------------------------

default_openrouter_credentials <- function(error_call = caller_env()) {
  if (key_exists("OPENROUTER_API_KEY")) {
    return(\() key_get("OPENROUTER_API_KEY"))
  }

  # A key obtained by logging in is cached on disk, so a non-interactive
  # session on the same machine (e.g. Rscript or a render) can reuse it
  # without being able to log in.
  client <- openrouter_oauth_client()
  if (is_interactive() || openrouter_oauth_has_cache(client)) {
    checked <- FALSE
    return(function() {
      # The cached key never expires, so httr2 would replay a revoked one
      # forever. Check it with OpenRouter once per chat object instead, and
      # clear it so the user is logged in again.
      if (!checked && openrouter_oauth_has_cache(client)) {
        cached <- oauth_token_cached(
          client,
          openrouter_oauth_flow,
          cache_disk = TRUE
        )
        if (!openrouter_key_is_valid(cached$access_token)) {
          openrouter_oauth_reset()
          if (!is_interactive()) {
            cli::cli_abort(
              c(
                "Your saved OpenRouter login is no longer valid.",
                i = "Call {.fn chat_openrouter} in an interactive session to log in again."
              ),
              call = error_call
            )
          }
          cli::cli_inform(
            "Your saved OpenRouter login is no longer valid; logging in again."
          )
        }
      }
      checked <<- TRUE
      token <- oauth_token_cached(
        client,
        openrouter_oauth_flow,
        cache_disk = TRUE
      )
      token$access_token
    })
  }

  if (is_testing()) {
    testthat::skip("OPENROUTER_API_KEY env var is not configured")
  }
  cli::cli_abort(
    c(
      "No OpenRouter credentials are available.",
      i = "Set {.envvar OPENROUTER_API_KEY}, or call {.fn chat_openrouter} in an interactive session to log in."
    ),
    call = error_call
  )
}

openrouter_oauth_client <- function() {
  oauth_client(
    id = "ellmer",
    # Required by oauth_client(), but unused: openrouter_oauth_flow() does its
    # own exchange and the key never needs refreshing.
    token_url = "https://openrouter.ai/api/v1/auth/keys",
    name = "ellmer-openrouter"
  )
}

openrouter_oauth_has_cache <- function(client) {
  cache_dir <- file.path(oauth_cache_path(), client$name)
  length(dir(cache_dir, pattern = "token")) > 0
}

openrouter_oauth_reset <- function() {
  httr2::oauth_cache_clear(openrouter_oauth_client(), cache_disk = TRUE)
}

openrouter_key_is_valid <- function(key) {
  req <- request("https://openrouter.ai/api/v1/auth/key")
  req <- req_auth_bearer_token(req, key)
  req <- ellmer_req_user_agent(req)
  req <- req_error(req, is_error = \(resp) FALSE)
  resp_status(req_perform(req)) != 401
}

# OpenRouter's login is PKCE-shaped but isn't standard OAuth 2.0: there's no
# client registration or scopes, and the code is exchanged for a long-lived
# API key rather than an access token. So we assemble the flow from httr2's
# building blocks rather than using req_oauth_auth_code(), and wrap the key as
# a token without an expiry so httr2 caches it but never tries to refresh it.
# https://openrouter.ai/docs/guides/overview/auth/oauth
openrouter_oauth_flow <- function(client, headless = is_hosted_session()) {
  pkce <- oauth_flow_auth_code_pkce()

  if (headless) {
    # Without a callback_url OpenRouter shows the code on screen instead.
    utils::browseURL(openrouter_auth_url(pkce))
    code <- trimws(readline("Enter the code shown by OpenRouter: "))
  } else {
    check_installed("httpuv", "for browser-based authentication")
    redirect_uri <- openrouter_redirect_uri()
    # Any unguessable string will do for `state`; reuse the PKCE generator.
    state <- oauth_flow_auth_code_pkce()$verifier
    utils::browseURL(openrouter_auth_url(pkce, state, redirect_uri))
    query <- oauth_flow_auth_code_listen(redirect_uri)
    code <- oauth_flow_auth_code_parse(query, state)
  }

  openrouter_exchange_code(code, pkce$verifier)
}

# httr2's default redirect uri has no port; pick a free one like its own flow
# does so the listener and the callback agree.
openrouter_redirect_uri <- function() {
  redirect_uri <- oauth_redirect_uri()
  if (is.null(url_parse(redirect_uri)$port)) {
    redirect_uri <- url_modify(redirect_uri, port = httpuv::randomPort())
  }
  redirect_uri
}

openrouter_auth_url <- function(pkce, state = NULL, callback_url = NULL) {
  url_modify_query(
    "https://openrouter.ai/auth",
    callback_url = callback_url,
    code_challenge = pkce$challenge,
    code_challenge_method = pkce$method,
    key_label = "ellmer",
    state = state
  )
}

openrouter_exchange_code <- function(code, verifier) {
  req <- request("https://openrouter.ai/api/v1/auth/keys")
  req <- req_body_json(
    req,
    list(
      code = code,
      code_verifier = verifier,
      code_challenge_method = "S256"
    )
  )
  req <- ellmer_req_user_agent(req)
  # OpenRouter's auth errors use the same {error: {message}} shape as its API.
  req <- req_error(req, body = openai_error_body)
  resp <- req_perform(req)

  oauth_token(access_token = resp_body_json(resp)$key)
}

method(base_request, ProviderOpenRouter) <- function(provider) {
  req <- base_request(super(provider, ProviderOpenAICompatible))
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
  model,
  result,
  has_type = FALSE
) {
  # https://openrouter.ai/docs/errors
  check_openrouter_error(result$error)

  value_turn(
    super(provider, ProviderOpenAICompatible),
    model = model,
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
    c(message, i = if (!is.null(details)) details),
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
  x,
  ...
) {
  if (identical(x@text, "")) {
    # Tool call requests can include a Content with empty text,
    # but it doesn't like it if you send this back
    NULL
  } else {
    list(type = "text", text = x@text)
  }
}
