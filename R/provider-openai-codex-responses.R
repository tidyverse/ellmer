#' @include provider-openai.R
NULL

# Public OAuth native-app client id for OpenAI's Codex CLI-compatible
# ChatGPT subscription flow, as used by Codex-aware tools such as pi-mono.
# This identifies the OAuth app/flow; it is not a user secret. Never commit
# access tokens, refresh tokens, or locally stored OAuth credential files.
openai_codex_client_id <- "app_EMoamEEZ73f0CkXaXp7hrann"
openai_codex_auth_base_url <- "https://auth.openai.com"
openai_codex_token_url <- paste0(openai_codex_auth_base_url, "/oauth/token")
openai_codex_device_user_code_url <- paste0(
  openai_codex_auth_base_url,
  "/api/accounts/deviceauth/usercode"
)
openai_codex_device_token_url <- paste0(
  openai_codex_auth_base_url,
  "/api/accounts/deviceauth/token"
)
openai_codex_device_verification_uri <- paste0(
  openai_codex_auth_base_url,
  "/codex/device"
)
openai_codex_device_redirect_uri <- paste0(
  openai_codex_auth_base_url,
  "/deviceauth/callback"
)
openai_codex_jwt_claim_path <- "https://api.openai.com/auth"

#' Chat with an OpenAI Codex Responses model using ChatGPT subscription auth
#'
#' @description
#' `r support_badge("community")`
#' `r lifecycle::badge("experimental")`
#'
#' This provider uses the ChatGPT Codex backend (`/codex/responses`) rather
#' than the public OpenAI developer API. It is intended for ChatGPT Plus/Pro
#' subscription authentication, similar to how Codex-aware CLI tools authenticate
#' with ChatGPT.
#'
#' Unlike [chat_openai()], this function does not use `OPENAI_API_KEY` and does
#' not bill the OpenAI developer platform. Instead, provide a credentials function
#' created by [openai_codex_credentials()] or [openai_codex_login()].
#'
#' @param credentials Credentials to use for authentication. Use
#'   [openai_codex_credentials()] to read existing OAuth tokens, or
#'   [openai_codex_login()] to start a device-code login flow.
#' @param model The Codex model to use for the chat (defaults to `"gpt-5.5"`).
#' @param service_tier Request a specific service tier. Use `"auto"` to omit the
#'   service tier and let the ChatGPT Codex backend choose the default.
#' @param account_id Optional ChatGPT account id. Usually extracted from the
#'   access token automatically.
#' @param text_verbosity Codex text verbosity, one of `"low"`, `"medium"`, or
#'   `"high"`.
#' @param originator Value for the `originator` request header.
#' @inheritParams chat_openai
#' @inherit chat_openai return
#' @family chatbots
#' @export
#' @examples
#' \dontrun{
#' # Interactive device-code login for the current R session:
#' creds <- openai_codex_login()
#' chat <- chat_openai_codex_responses(credentials = creds)
#' chat$chat("Write a small R function that adds two numbers")
#'
#' # Or use env vars, especially OPENAI_CODEX_REFRESH_TOKEN:
#' chat <- chat_openai_codex_responses()
#' }
chat_openai_codex_responses <- function(
  system_prompt = NULL,
  base_url = "https://chatgpt.com/backend-api",
  credentials = NULL,
  model = NULL,
  params = NULL,
  api_args = list(),
  api_headers = character(),
  service_tier = c("auto", "default", "flex", "priority"),
  account_id = NULL,
  text_verbosity = c("low", "medium", "high"),
  originator = "ellmer",
  echo = c("none", "output", "all")
) {
  model <- set_default(model, "gpt-5.5")
  echo <- check_echo(echo)
  service_tier <- arg_match(service_tier)
  text_verbosity <- arg_match(text_verbosity)
  check_string(account_id, allow_null = TRUE)
  check_string(originator, allow_empty = FALSE)

  credentials <- as_credentials(
    "chat_openai_codex_responses",
    default_openai_codex_credentials(),
    credentials = credentials
  )

  provider <- ProviderOpenAICodexResponses(
    name = "OpenAI Codex Responses",
    base_url = base_url,
    model = model,
    params = params %||% params(),
    extra_args = api_args,
    extra_headers = api_headers,
    credentials = credentials,
    service_tier = service_tier,
    account_id = account_id,
    text_verbosity = text_verbosity,
    originator = originator
  )
  Chat$new(provider = provider, system_prompt = system_prompt, echo = echo)
}

ProviderOpenAICodexResponses <- new_class(
  "ProviderOpenAICodexResponses",
  parent = ProviderOpenAI,
  properties = list(
    account_id = prop_string(allow_null = TRUE),
    text_verbosity = class_character,
    originator = class_character
  )
)

# Auth -------------------------------------------------------------------------

#' OpenAI Codex subscription credentials
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Creates a zero-argument credentials function suitable for
#' [chat_openai_codex_responses()]. If a refresh token is supplied, the access
#' token is refreshed automatically when needed.
#'
#' By default, credentials are read from:
#' * `OPENAI_CODEX_ACCESS_TOKEN`
#' * `OPENAI_CODEX_REFRESH_TOKEN`
#' * `OPENAI_CODEX_EXPIRES_AT` (Unix timestamp, seconds)
#' * `OPENAI_CODEX_ACCOUNT_ID`
#'
#' @param access_token OAuth access token. If omitted, read from
#'   `OPENAI_CODEX_ACCESS_TOKEN`.
#' @param refresh_token OAuth refresh token. If omitted, read from
#'   `OPENAI_CODEX_REFRESH_TOKEN`.
#' @param expires_at Expiry time as a Unix timestamp in seconds, or `NULL` if
#'   unknown.
#' @param account_id ChatGPT account id. Usually extracted from the access token.
#' @return A zero-argument credentials function.
#' @export
openai_codex_credentials <- function(
  access_token = Sys.getenv("OPENAI_CODEX_ACCESS_TOKEN", ""),
  refresh_token = Sys.getenv("OPENAI_CODEX_REFRESH_TOKEN", ""),
  expires_at = openai_codex_env_number("OPENAI_CODEX_EXPIRES_AT"),
  account_id = Sys.getenv("OPENAI_CODEX_ACCOUNT_ID", "")
) {
  if (identical(access_token, "")) {
    access_token <- NULL
  }
  if (identical(refresh_token, "")) {
    refresh_token <- NULL
  }
  if (identical(account_id, "")) {
    account_id <- NULL
  }

  state <- new.env(parent = emptyenv())
  state$access_token <- access_token
  state$refresh_token <- refresh_token
  state$expires_at <- openai_codex_normalize_expires(expires_at)
  state$account_id <- account_id

  credentials <- function() {
    openai_codex_refresh_if_needed(state)

    if (is.null(state$access_token)) {
      cli::cli_abort(c(
        "No OpenAI Codex credentials are available.",
        "i" = "Run {.fn openai_codex_login} or set {.envvar OPENAI_CODEX_REFRESH_TOKEN}."
      ))
    }

    account_id <- state$account_id %||%
      openai_codex_account_id(state$access_token)
    if (is.null(account_id)) {
      cli::cli_abort(
        "Can't determine ChatGPT account id from {.envvar OPENAI_CODEX_ACCESS_TOKEN}."
      )
    }
    state$account_id <- account_id

    list(
      Authorization = paste("Bearer", state$access_token),
      `chatgpt-account-id` = account_id
    )
  }

  attr(credentials, "token") <- function() {
    list(
      access = state$access_token,
      refresh = state$refresh_token,
      expires = state$expires_at,
      account_id = state$account_id
    )
  }
  credentials
}

#' @rdname openai_codex_credentials
#' @param as_credentials If `TRUE`, return a credentials function. If `FALSE`,
#'   return the raw token list with `access`, `refresh`, `expires`, and
#'   `account_id` fields.
#' @param poll_interval Minimum polling interval in seconds.
#' @param timeout Maximum time to wait for device authorization, in seconds.
#' @export
openai_codex_login <- function(
  as_credentials = TRUE,
  poll_interval = 5,
  timeout = 15 * 60
) {
  check_bool(as_credentials)
  check_number_decimal(poll_interval, min = 0)
  check_number_decimal(timeout, min = 1)

  device <- openai_codex_start_device_auth()
  interval <- max(device$interval, poll_interval)

  cli::cli_inform(c(
    "OpenAI Codex device login started.",
    "i" = "Open {.url {openai_codex_device_verification_uri}}",
    "i" = "Enter code: {.strong {device$user_code}}"
  ))

  code <- openai_codex_poll_device_auth(device, interval, timeout)
  token <- openai_codex_exchange_code(
    code$authorization_code,
    code$code_verifier,
    redirect_uri = openai_codex_device_redirect_uri
  )

  if (!as_credentials) {
    return(token)
  }

  openai_codex_credentials(
    access_token = token$access,
    refresh_token = token$refresh,
    expires_at = token$expires,
    account_id = token$account_id
  )
}

#' @rdname openai_codex_credentials
#' @param refresh_token OAuth refresh token.
#' @export
openai_codex_refresh <- function(
  refresh_token = Sys.getenv("OPENAI_CODEX_REFRESH_TOKEN")
) {
  check_string(refresh_token, allow_empty = FALSE)
  openai_codex_refresh_token(refresh_token)
}

default_openai_codex_credentials <- function(error_call = caller_env()) {
  if (
    key_exists("OPENAI_CODEX_ACCESS_TOKEN") ||
      key_exists("OPENAI_CODEX_REFRESH_TOKEN")
  ) {
    return(openai_codex_credentials())
  }

  function() {
    if (is_replaying()) {
      return(list(
        Authorization = "Bearer <OPENAI_CODEX_ACCESS_TOKEN>",
        `chatgpt-account-id` = "acct_test"
      ))
    }
    if (is_testing()) {
      testthat::skip(
        "OPENAI_CODEX_ACCESS_TOKEN/OPENAI_CODEX_REFRESH_TOKEN env var is not configured"
      )
    }
    cli::cli_abort(
      c(
        "No OpenAI Codex credentials are available.",
        "i" = "Run {.fn openai_codex_login} or set {.envvar OPENAI_CODEX_REFRESH_TOKEN}."
      ),
      call = error_call
    )
  }
}

openai_codex_normalize_expires <- function(expires) {
  if (is.null(expires)) {
    return(NULL)
  }
  expires <- as.numeric(expires)
  if (is.na(expires)) {
    return(NULL)
  }
  # Some OAuth stores record expiry in milliseconds; ellmer uses Unix seconds.
  if (expires > 1e12) {
    expires <- expires / 1000
  }
  expires
}

openai_codex_refresh_if_needed <- function(state) {
  needs_refresh <- is.null(state$access_token) ||
    (!is.null(state$expires_at) &&
      state$expires_at <= as.numeric(Sys.time()) + 30)

  if (!needs_refresh) {
    return(invisible())
  }
  if (is.null(state$refresh_token)) {
    return(invisible())
  }

  token <- openai_codex_refresh_token(state$refresh_token)
  state$access_token <- token$access
  state$refresh_token <- token$refresh
  state$expires_at <- token$expires
  state$account_id <- token$account_id
  invisible()
}

openai_codex_start_device_auth <- function() {
  req <- request(openai_codex_device_user_code_url)
  req <- req_method(req, "POST")
  req <- req_body_json(req, list(client_id = openai_codex_client_id))

  resp <- req_perform(req)
  json <- resp_body_json(resp, check_type = FALSE)
  interval <- as.numeric(json$interval %||% 5)

  if (
    !is_string(json$device_auth_id) ||
      !is_string(json$user_code) ||
      is.na(interval)
  ) {
    cli::cli_abort("Invalid OpenAI Codex device code response.")
  }

  list(
    device_auth_id = json$device_auth_id,
    user_code = json$user_code,
    interval = interval
  )
}

openai_codex_poll_device_auth <- function(device, interval, timeout) {
  deadline <- Sys.time() + timeout

  repeat {
    if (Sys.time() > deadline) {
      cli::cli_abort("OpenAI Codex device login timed out.")
    }

    Sys.sleep(interval)

    req <- request(openai_codex_device_token_url)
    req <- req_method(req, "POST")
    req <- req_body_json(
      req,
      list(
        device_auth_id = device$device_auth_id,
        user_code = device$user_code
      )
    )

    resp <- tryCatch(req_perform(req), httr2_error = identity)
    if (!inherits(resp, "httr2_error")) {
      json <- resp_body_json(resp, check_type = FALSE)
      if (is_string(json$authorization_code) && is_string(json$code_verifier)) {
        return(list(
          authorization_code = json$authorization_code,
          code_verifier = json$code_verifier
        ))
      }
      cli::cli_abort("Invalid OpenAI Codex device token response.")
    }

    response <- resp$resp
    status <- response$status_code %||% response$status
    if (status %in% c(403, 404)) {
      next
    }

    body <- tryCatch(
      resp_body_json(response, check_type = FALSE),
      error = function(cnd) NULL
    )
    error <- body$error
    code <- if (is.list(error)) error$code else error
    if (identical(code, "deviceauth_authorization_pending")) {
      next
    }
    if (identical(code, "slow_down")) {
      interval <- interval + 5
      next
    }

    cli::cli_abort("OpenAI Codex device login failed: {resp$message}")
  }
}

openai_codex_exchange_code <- function(
  code,
  verifier,
  redirect_uri = openai_codex_device_redirect_uri
) {
  req <- request(openai_codex_token_url)
  req <- req_method(req, "POST")
  req <- req_body_form(
    req,
    grant_type = "authorization_code",
    client_id = openai_codex_client_id,
    code = code,
    code_verifier = verifier,
    redirect_uri = redirect_uri
  )

  resp <- req_perform(req)
  openai_codex_token_from_response(resp)
}

openai_codex_refresh_token <- function(refresh_token) {
  req <- request(openai_codex_token_url)
  req <- req_method(req, "POST")
  req <- req_body_form(
    req,
    grant_type = "refresh_token",
    refresh_token = refresh_token,
    client_id = openai_codex_client_id
  )

  resp <- req_perform(req)
  openai_codex_token_from_response(resp)
}

openai_codex_token_from_response <- function(resp) {
  json <- resp_body_json(resp, check_type = FALSE)
  if (
    !is_string(json$access_token) ||
      !is_string(json$refresh_token) ||
      is.null(json$expires_in)
  ) {
    cli::cli_abort("OpenAI Codex token response is missing required fields.")
  }

  access <- json$access_token
  account_id <- openai_codex_account_id(access)
  if (is.null(account_id)) {
    cli::cli_abort(
      "Failed to extract ChatGPT account id from OpenAI Codex token."
    )
  }

  list(
    access = access,
    refresh = json$refresh_token,
    expires = as.numeric(Sys.time()) + as.numeric(json$expires_in),
    account_id = account_id
  )
}

openai_codex_env_number <- function(name) {
  value <- Sys.getenv(name, "")
  if (identical(value, "")) {
    NULL
  } else {
    as.numeric(value)
  }
}

openai_codex_account_id <- function(token) {
  payload <- openai_codex_jwt_payload(token)
  account_id <- payload[[openai_codex_jwt_claim_path]]$chatgpt_account_id
  if (is_string(account_id) && nzchar(account_id)) account_id else NULL
}

openai_codex_jwt_payload <- function(token) {
  parts <- strsplit(token, ".", fixed = TRUE)[[1]]
  if (length(parts) != 3) {
    cli::cli_abort("Invalid OpenAI Codex access token.")
  }

  payload <- parts[[2]]
  payload <- gsub("\\s+", "", payload)
  payload <- chartr("-_", "+/", payload)
  padding <- (4 - nchar(payload) %% 4) %% 4
  if (padding) {
    payload <- paste0(payload, strrep("=", padding))
  }

  json <- rawToChar(jsonlite::base64_dec(payload))
  jsonlite::fromJSON(json, simplifyVector = FALSE)
}

# Base request -----------------------------------------------------------------

method(base_request, ProviderOpenAICodexResponses) <- function(provider) {
  req <- request(provider@base_url)
  req <- openai_codex_req_credentials(
    req,
    provider@credentials(),
    account_id = provider@account_id
  )
  req <- ellmer_req_robustify(req)
  req <- req_headers_redacted(
    req,
    `OpenAI-Beta` = "responses=experimental",
    originator = provider@originator
  )
  req <- ellmer_req_user_agent(req)
  req <- base_request_error(provider, req)
  req
}

openai_codex_req_credentials <- function(req, credentials, account_id = NULL) {
  if (is.function(credentials)) {
    return(credentials(req))
  }

  if (is_string(credentials)) {
    token <- credentials
    account_id <- account_id %||% openai_codex_account_id(token)
    credentials <- list(
      Authorization = paste("Bearer", token),
      `chatgpt-account-id` = account_id
    )
  }

  if (!has_name(credentials, "chatgpt-account-id")) {
    authorization <- credentials$Authorization %||% credentials$authorization
    if (is_string(authorization)) {
      token <- sub("^Bearer\\s+", "", authorization, ignore.case = TRUE)
      credentials$`chatgpt-account-id` <- account_id %||%
        openai_codex_account_id(token)
    }
  }

  req_headers_redacted(req, !!!credentials)
}

# Chat endpoint ----------------------------------------------------------------

method(chat_path, ProviderOpenAICodexResponses) <- function(provider) {
  "/codex/responses"
}

method(chat_body, ProviderOpenAICodexResponses) <- function(
  provider,
  stream = TRUE,
  turns = list(),
  tools = list(),
  type = NULL
) {
  if (length(turns) >= 1 && is_system_turn(turns[[1]])) {
    instructions <- turns[[1]]@text
    turns <- turns[-1]
  } else {
    instructions <- "You are a helpful assistant."
  }

  input <- compact(unlist(as_json(provider, turns), recursive = FALSE))
  tools <- as_json(provider, unname(tools))

  if (!is.null(type)) {
    text <- list(
      verbosity = provider@text_verbosity,
      format = list(
        type = "json_schema",
        name = "structured_data",
        schema = as_json(provider, type),
        strict = TRUE
      )
    )
  } else {
    text <- list(verbosity = provider@text_verbosity)
  }

  params <- chat_params(provider, provider@params)
  if (has_name(params, "reasoning_effort")) {
    reasoning <- list(
      effort = params$reasoning_effort,
      summary = "auto"
    )
    params$reasoning_effort <- NULL
  } else {
    reasoning <- NULL
  }

  params$log_probs <- NULL

  compact(list2(
    model = provider@model,
    instructions = instructions,
    input = input,
    !!!params,
    stream = stream,
    tools = tools,
    text = text,
    reasoning = reasoning,
    include = list("reasoning.encrypted_content"),
    store = FALSE,
    service_tier = if (provider@service_tier != "auto") provider@service_tier,
    tool_choice = if (length(tools) > 0) "auto",
    parallel_tool_calls = if (length(tools) > 0) TRUE
  ))
}

method(chat_request, ProviderOpenAICodexResponses) <- function(
  provider,
  stream = TRUE,
  turns = list(),
  tools = list(),
  type = NULL
) {
  req <- base_request(provider)
  req <- req_url_path_append(req, chat_path(provider))

  body <- chat_body(
    provider = provider,
    stream = stream,
    turns = turns,
    tools = tools,
    type = type
  )
  body <- modify_list(body, provider@extra_args)
  req <- req_body_json(req, body)
  req <- req_headers(req, !!!provider@extra_headers)
  if (stream) {
    req <- req_headers(req, accept = "text/event-stream")
  }

  req
}

method(stream_merge_chunks, ProviderOpenAICodexResponses) <- function(
  provider,
  result,
  chunk
) {
  if (chunk$type %in% c("response.created", "response.in_progress")) {
    return(result %||% chunk$response)
  }

  if (chunk$type == "response.output_item.added") {
    result <- result %||% list(output = list())
    result$output[[chunk$output_index + 1]] <- chunk$item
    return(result)
  }

  if (chunk$type == "response.function_call_arguments.delta") {
    result <- result %||% list(output = list())
    idx <- chunk$output_index + 1
    result$output[[idx]]$arguments <- paste0(
      result$output[[idx]]$arguments %||% "",
      chunk$delta %||% ""
    )
    return(result)
  }

  if (chunk$type == "response.function_call_arguments.done") {
    result <- result %||% list(output = list())
    result$output[[chunk$output_index + 1]]$arguments <- chunk$arguments
    return(result)
  }

  if (chunk$type == "response.output_item.done") {
    result <- result %||% list(output = list())
    result$output[[chunk$output_index + 1]] <- chunk$item
    return(result)
  }

  if (
    chunk$type %in%
      c("response.done", "response.completed", "response.incomplete")
  ) {
    response <- chunk$response
    if (length(response$output) == 0 && length(result$output %||% list()) > 0) {
      response$output <- result$output
    }
    return(response)
  }

  if (chunk$type == "response.failed") {
    error <- chunk$response$error
    cli::cli_abort(c("Request failed ({error$code})", "{error$message}"))
  } else if (chunk$type == "error") {
    message <- chunk$message %||%
      chunk$error$message %||%
      chunk$error$code %||%
      "Unknown Codex error"
    cli::cli_abort("Request errored: {message}")
  }

  result
}

method(value_turn, ProviderOpenAICodexResponses) <- function(
  provider,
  result,
  has_type = FALSE
) {
  turn <- value_turn(
    super(provider, ProviderOpenAI),
    result = result,
    has_type = has_type
  )
  turn@cost <- openai_codex_token_cost(
    provider@model,
    as.list(turn@tokens),
    result$service_tier
  )
  turn
}

openai_codex_token_cost <- function(model, tokens, service_tier = NULL) {
  costs <- list(
    `gpt-5.3-codex-spark` = c(input = 1.75, output = 14, cached_input = 0.175),
    `gpt-5.4` = c(input = 2.5, output = 15, cached_input = 0.25),
    `gpt-5.4-mini` = c(input = 0.75, output = 4.5, cached_input = 0.075),
    `gpt-5.5` = c(input = 5, output = 30, cached_input = 0.5)
  )
  cost <- costs[[model]]
  if (is.null(cost)) {
    return(dollars(NA_real_))
  }

  multiplier <- switch(
    service_tier %||% "default",
    flex = 0.5,
    priority = if (identical(model, "gpt-5.5")) 2.5 else 2,
    1
  )

  dollars(
    multiplier *
      (tokens$input *
        cost[["input"]] /
        1e6 +
        tokens$output * cost[["output"]] / 1e6 +
        tokens$cached_input * cost[["cached_input"]] / 1e6)
  )
}

method(as_json, list(ProviderOpenAICodexResponses, ToolDef)) <- function(
  provider,
  x,
  ...
) {
  list(
    type = "function",
    name = x@name,
    description = x@description,
    parameters = as_json(provider, x@arguments, ...)
  )
}
