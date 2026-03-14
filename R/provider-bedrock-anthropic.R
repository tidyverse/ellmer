#' @include provider-claude.R
#' @include provider-aws.R
NULL

#' Chat with Anthropic Claude on AWS Bedrock (native API)
#'
#' @description
#' Uses the Anthropic Messages API format via Bedrock's
#' [InvokeModel](https://docs.aws.amazon.com/bedrock/latest/APIReference/API_runtime_InvokeModel.html)
#' endpoint, rather than the
#' [Converse](https://docs.aws.amazon.com/bedrock/latest/APIReference/API_runtime_Converse.html)
#' API. This enables Anthropic-specific features like
#' [prompt caching](https://docs.anthropic.com/en/docs/build-with-claude/prompt-caching),
#' which is not available through the Converse API.
#'
#' For most use cases, [chat_aws_bedrock()] is the recommended way to use
#' Bedrock. Use `chat_bedrock_anthropic()` when you need prompt caching or
#' other Anthropic-native features.
#'
#' @inheritParams chat_aws_bedrock
#' @param cache How long to cache inputs? Defaults to `"5m"` (five minutes).
#'   Set to `"none"` to disable caching or `"1h"` to cache for one hour.
#'   See [chat_anthropic()] for details on how caching works.
#' @inherit chat_openai return
#' @family chatbots
#' @export
#' @examples
#' \dontrun{
#' chat <- chat_bedrock_anthropic()
#' chat$chat("Tell me three jokes about statisticians")
#' }
chat_bedrock_anthropic <- function(
  system_prompt = NULL,
  base_url = NULL,
  model = NULL,
  profile = NULL,
  cache = c("5m", "1h", "none"),
  params = NULL,
  api_args = list(),
  api_headers = character(),
  echo = NULL
) {
  check_installed("paws.common", "AWS authentication")
  check_string(base_url, allow_null = TRUE)
  base_url <- base_url %||%
    \(x) sprintf("https://bedrock-runtime.%s.amazonaws.com", x)
  echo <- check_echo(echo)

  cache <- arg_match(cache)
  params <- params %||% params()

  provider <- provider_bedrock_anthropic(
    base_url = base_url,
    model = model,
    profile = profile,
    cache = cache,
    params = params,
    extra_args = api_args,
    extra_headers = api_headers
  )
  Chat$new(provider = provider, system_prompt = system_prompt, echo = echo)
}

provider_bedrock_anthropic <- function(
  base_url,
  model = "",
  profile = NULL,
  cache = "5m",
  params = list(),
  extra_args = list(),
  extra_headers = character()
) {
  creds_cache <- aws_creds_cache(profile)
  credentials <- paws_credentials(profile, cache = creds_cache)

  if (is.function(base_url)) {
    base_url <- base_url(credentials$region)
  }

  model <- set_default(model, "anthropic.claude-sonnet-4-5-20250929-v1:0")

  ProviderBedrockAnthropic(
    name = "AWS/Bedrock (Anthropic)",
    base_url = base_url,
    model = model,
    # ProviderAnthropic properties
    beta_headers = character(),
    credentials = NULL,
    cache = cache,
    # Bedrock-specific properties
    profile = profile,
    region = credentials$region,
    creds_cache = creds_cache,
    params = params,
    extra_args = extra_args,
    extra_headers = extra_headers
  )
}

ProviderBedrockAnthropic <- new_class(
  "ProviderBedrockAnthropic",
  parent = ProviderAnthropic,
  properties = list(
    profile = prop_string(allow_null = TRUE),
    region = prop_string(),
    creds_cache = class_list
  )
)

# Authentication: use AWS SigV4 instead of API key
method(base_request, ProviderBedrockAnthropic) <- function(provider) {
  creds <- paws_credentials(provider@profile, provider@creds_cache)

  req <- request(provider@base_url)
  req <- req_auth_aws_v4(
    req,
    aws_access_key_id = creds$access_key_id,
    aws_secret_access_key = creds$secret_access_key,
    aws_session_token = creds$session_token
  )
  req <- ellmer_req_robustify(req)
  req <- ellmer_req_user_agent(req)
  req <- base_request_error(provider, req)
  req
}

method(base_request_error, ProviderBedrockAnthropic) <- function(
  provider,
  req
) {
  req_error(req, body = function(resp) {
    body <- resp_body_json(resp)
    body$Message %||% body$message
  })
}

# Use Bedrock invoke_model / invoke-with-response-stream endpoints
method(chat_request, ProviderBedrockAnthropic) <- function(
  provider,
  stream = TRUE,
  turns = list(),
  tools = list(),
  type = NULL
) {
  req <- base_request(provider)
  endpoint <- if (stream) "invoke-with-response-stream" else "invoke"
  req <- req_url_path_append(req, "model", provider@model, endpoint)

  # Build the Anthropic Messages API body (inherited from ProviderAnthropic)
  body <- chat_body(
    provider = provider,
    stream = FALSE, # Anthropic stream param not needed for Bedrock
    turns = turns,
    tools = tools,
    type = type
  )

  # Add Bedrock-required anthropic_version
  body$anthropic_version <- "bedrock-2023-05-31"

  # Remove fields that Bedrock doesn't accept in the body.
  # model is specified in the URL path, not the request body.
  body$model <- NULL
  body$stream <- NULL

  # Merge extra_args
  body <- modify_list(body, provider@extra_args)

  req <- req_body_json(req, body)
  req <- req_headers(req, !!!provider@extra_headers)

  req
}

# Bedrock uses AWS event stream format for streaming
method(chat_resp_stream, ProviderBedrockAnthropic) <- function(provider, resp) {
  resp_stream_aws(resp)
}

# Parse AWS event stream events containing Anthropic JSON payloads
method(stream_parse, ProviderBedrockAnthropic) <- function(provider, event) {
  if (is.null(event)) {
    cli::cli_abort("Connection closed unexpectedly")
  }

  # InvokeModelWithResponseStream wraps Anthropic events in AWS event stream.
  # event$body$bytes contains the raw Anthropic JSON payload.
  bytes <- event$body$bytes
  if (is.null(bytes)) {
    return(NULL)
  }

  if (is.raw(bytes)) {
    json_str <- rawToChar(bytes)
  } else {
    # base64-encoded string
    json_str <- rawToChar(jsonlite::base64_dec(bytes))
  }

  data <- jsonlite::parse_json(json_str)
  if (identical(data$type, "message_stop")) {
    return(NULL)
  }

  data
}

# Token tracking: same as ProviderAnthropic (inherited), since invoke_model
# returns Anthropic-format usage with cache_read_input_tokens etc.
