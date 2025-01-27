#' @include provider-openai.R
#' @include content.R
NULL

#' Chat with a model hosted on Snowflake
#'
#' @description
#' The Snowflake provider allows you to interact with LLM models available
#' through the [Cortex LLM REST API](https://docs.snowflake.com/en/user-guide/snowflake-cortex/cortex-llm-rest-api).
#'
#' ## Authentication
#'
#' `chat_snowflake()` picks up the following ambient Snowflake credentials:
#'
#' - A static OAuth token defined via the `SNOWFLAKE_TOKEN` environment
#'   variable.
#' - Key-pair authentication credentials defined via the `SNOWFLAKE_USER` and
#'   `SNOWFLAKE_PRIVATE_KEY` (which can be a PEM-encoded private key or a path
#'   to one) environment variables.
#' - Posit Workbench-managed Snowflake credentials for the corresponding
#'   `account`.
#'
#' ## Known limitations
#' Note that Snowflake-hosted models do not support images, tool calling, or
#' structured outputs.
#'
#' See [chat_cortex()] to chat with the Snowflake Cortex Analyst rather than a
#' general-purpose model.
#'
#' @inheritParams chat_openai
#' @inheritParams chat_cortex
#' @inherit chat_openai return
#' @examplesIf has_credentials("cortex")
#' chat <- chat_snowflake()
#' chat$chat("Tell me a joke in the form of a SQL query.")
#' @export
chat_snowflake <- function(system_prompt = NULL,
                           turns = NULL,
                           account = snowflake_account(),
                           credentials = NULL,
                           model = NULL,
                           api_args = list(),
                           echo = c("none", "text", "all")) {
  turns <- normalize_turns(turns, system_prompt)
  check_string(account, allow_empty = FALSE)
  model <- set_default(model, "llama3.1-70b")
  echo <- check_echo(echo)

  if (is_list(credentials)) {
    static_credentials <- force(credentials)
    credentials <- function(account) static_credentials
  }
  check_function(credentials, allow_null = TRUE)

  provider <- ProviderSnowflake(
    base_url = snowflake_url(account),
    account = account,
    credentials = credentials,
    model = model,
    extra_args = api_args,
    # We need an empty api_key for S7 validation.
    api_key = ""
  )

  Chat$new(provider = provider, turns = turns, echo = echo)
}

ProviderSnowflake <- new_class(
  "ProviderSnowflake",
  parent = ProviderOpenAI,
  properties = list(
    account = prop_string(),
    credentials = class_function | NULL
  )
)

# See: https://docs.snowflake.com/en/user-guide/snowflake-cortex/cortex-llm-rest-api#api-reference
method(chat_request, ProviderSnowflake) <- function(provider,
                                                    stream = TRUE,
                                                    turns = list(),
                                                    tools = list(),
                                                    type = NULL,
                                                    extra_args = list()) {
  if (length(tools) != 0) {
    cli::cli_abort(
      "Tool calling is not supported.",
      call = quote(chat_snowflake())
    )
  }
  if (!is.null(type) != 0) {
    cli::cli_abort(
      "Structured data extraction is not supported.",
      call = quote(chat_snowflake())
    )
  }
  if (!stream) {
    cli::cli_abort(
      "Non-streaming responses are not supported.",
      call = quote(chat_snowflake())
    )
  }

  req <- request(provider@base_url)
  req <- req_url_path_append(req, "/api/v2/cortex/inference:complete")
  creds <- cortex_credentials(provider@account, provider@credentials)
  req <- req_headers(req, !!!creds, .redact = "Authorization")
  req <- req_retry(req, max_tries = 2)
  req <- req_timeout(req, 60)
  req <- req_user_agent(req, snowflake_user_agent())

  # Snowflake-specific error response handling:
  req <- req_error(req, body = function(resp) resp_body_json(resp)$message)

  messages <- as_json(provider, turns)
  extra_args <- utils::modifyList(provider@extra_args, extra_args)

  data <- compact(list2(
    messages = messages,
    model = provider@model,
    stream = stream,
    !!!extra_args
  ))
  req <- req_body_json(req, data)

  req
}

# Snowflake -> ellmer --------------------------------------------------------

method(stream_parse, ProviderSnowflake) <- function(provider, event) {
  # Snowflake's SSEs look much like the OpenAI ones, except in their
  # handling of EOF.
  if (is.null(event)) {
    # This seems to be how Snowflake's backend signals that the stream is done.
    return(NULL)
  }
  jsonlite::parse_json(event$data)
}

method(value_turn, ProviderSnowflake) <- function(provider, result, has_type = FALSE) {
  deltas <- compact(sapply(result$choices, function(x) x$delta$content))
  content <- list(as_content(paste(deltas, collapse = "")))
  tokens <- c(
    result$usage$prompt_tokens %||% NA_integer_,
    result$usage$completion_tokens %||% NA_integer_
  )
  tokens_log(paste0("Snowflake-", provider@account), tokens)
  Turn(
    # Snowflake's response format seems to omit the role.
    "assistant",
    content,
    json = result,
    tokens = tokens
  )
}

# ellmer -> Snowflake --------------------------------------------------------

# Snowflake only supports simple textual messages.

method(as_json, list(ProviderSnowflake, Turn)) <- function(provider, x) {
  list(
    role = x@role,
    content = as_json(provider, x@contents[[1]])
  )
}

method(as_json, list(ProviderSnowflake, ContentText)) <- function(provider, x) {
  x@text
}

# Utilities ------------------------------------------------------------------

snowflake_account <- function() {
  key_get("SNOWFLAKE_ACCOUNT")
}

snowflake_url <- function(account) {
  paste0("https://", account, ".snowflakecomputing.com")
}

# Snowflake uses the User Agent header to identify "parter applications", so
# identify requests as coming from "r_ellmer" (unless an explicit partner
# application is set via the ambient SF_PARTNER environment variable).
snowflake_user_agent <- function() {
  user_agent <- paste0("r_ellmer/", utils::packageVersion("ellmer"))
  if (nchar(Sys.getenv("SF_PARTNER")) != 0) {
    user_agent <- Sys.getenv("SF_PARTNER")
  }
  user_agent
}

snowflake_keypair_token <- function(
  account,
  user,
  key,
  cache = snowflake_keypair_cache(account, key),
  lifetime = 600L,
  reauth = FALSE
) {
  # Producing a signed JWT is a fairly expensive operation (in the order of
  # ~10ms), but adding a cache speeds this up approximately 500x.
  creds <- cache$get()
  if (reauth || is.null(creds) || creds$expiry < Sys.time()) {
    cache$clear()
    expiry <- Sys.time() + lifetime
    # We can't use openssl::fingerprint() here because it uses a different
    # algorithm.
    fp <- openssl::base64_encode(
      openssl::sha256(openssl::write_der(key$pubkey))
    )
    sub <- toupper(paste0(account, ".", user))
    iss <- paste0(sub, ".SHA256:", fp)
    # Note: Snowflake employs a malformed issuer claim, so we have to inject it
    # manually after jose's validation phase.
    claim <- jwt_claim("dummy", sub, exp = as.integer(expiry))
    claim$iss <- iss
    creds <- list(expiry = expiry, token = jwt_encode_sig(claim, key))
    cache$set(creds)
  }
  creds$token
}

snowflake_keypair_cache <- function(account, key) {
  credentials_cache(key = hash(c("sf", account, openssl::fingerprint(key))))
}
