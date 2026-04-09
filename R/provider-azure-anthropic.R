#' @include provider-azure.R
#' @include provider-claude.R
NULL

# https://learn.microsoft.com/en-us/azure/ai-services/models/get-started

#' Chat with an Anthropic Claude model hosted on Azure AI Foundry
#'
#' @description
#' [Azure AI Foundry](https://azure.microsoft.com/en-us/products/ai-foundry)
#' hosts Anthropic Claude models accessible via the
#' `*.services.ai.azure.com/anthropic` endpoint, using the Anthropic Messages
#' API format.
#'
#' Unlike [chat_azure_openai()], which targets `*.openai.azure.com` endpoints
#' and uses the OpenAI chat completions format, this function targets Azure AI
#' Foundry's Anthropic-compatible endpoint.
#'
#' ## Authentication
#'
#' `chat_azure_anthropic()` supports API keys via the `AZURE_ANTHROPIC_API_KEY`
#' environment variable and the `credentials` parameter. It also supports:
#'
#' - Azure service principals (when the `AZURE_TENANT_ID`, `AZURE_CLIENT_ID`,
#'   and `AZURE_CLIENT_SECRET` environment variables are set).
#' - Interactive Entra ID authentication, like the Azure CLI.
#' - Viewer-based credentials on Posit Connect. Requires the \pkg{connectcreds}
#'   package.
#'
#' @param endpoint Azure AI Foundry endpoint URL with protocol and hostname,
#'   i.e. `https://{your-project}.services.ai.azure.com/anthropic`. Defaults
#'   to the value of the `AZURE_ANTHROPIC_ENDPOINT` environment variable.
#' @param model The model name to use (e.g., `"claude-opus-4-5"`).
#'   Defaults to `"claude-sonnet-4-5-20250929"`.
#' @param api_version The API version to use.
#' @param credentials `r api_key_param("AZURE_ANTHROPIC_API_KEY")`
#' @inheritParams chat_anthropic
#' @inherit chat_openai return
#' @family chatbots
#' @export
#' @examples
#' \dontrun{
#' chat <- chat_azure_anthropic(
#'   endpoint = "https://your-project.services.ai.azure.com/anthropic",
#'   model = "claude-opus-4-5"
#' )
#' chat$chat("Tell me three jokes about statisticians")
#' }
chat_azure_anthropic <- function(
  endpoint = azure_anthropic_endpoint(),
  model = NULL,
  params = NULL,
  api_version = NULL,
  system_prompt = NULL,
  credentials = NULL,
  cache = c("5m", "1h", "none"),
  beta_headers = character(),
  api_args = list(),
  api_headers = character(),
  echo = NULL
) {
  check_string(endpoint)
  params <- params %||% params()
  api_version <- set_default(api_version, "2024-10-22")
  model <- set_default(model, "claude-sonnet-4-5-20250929")
  cache <- arg_match(cache)
  echo <- check_echo(echo)

  credentials <- as_credentials(
    "chat_azure_anthropic",
    default_azure_anthropic_credentials(),
    credentials = credentials
  )

  provider <- ProviderAzureAnthropic(
    name = "Azure/Anthropic",
    base_url = paste0(endpoint, "/v1"),
    model = model,
    params = params,
    api_version = api_version,
    credentials = credentials,
    extra_args = api_args,
    extra_headers = api_headers,
    beta_headers = beta_headers,
    cache = cache
  )

  Chat$new(provider = provider, system_prompt = system_prompt, echo = echo)
}

ProviderAzureAnthropic <- new_class(
  "ProviderAzureAnthropic",
  parent = ProviderAnthropic,
  properties = list(
    api_version = prop_string()
  )
)

azure_anthropic_endpoint <- function() {
  key_get("AZURE_ANTHROPIC_ENDPOINT")
}

# https://learn.microsoft.com/en-us/azure/ai-services/openai/reference
method(base_request, ProviderAzureAnthropic) <- function(provider) {
  req <- request(provider@base_url)
  req <- ellmer_req_robustify(req, is_transient = function(resp) {
    resp_status(resp) %in% c(429, 503, 529)
  })
  req <- ellmer_req_user_agent(req)
  # Azure AI Foundry uses api-key header (not x-api-key like standard Anthropic)
  req <- ellmer_req_credentials(req, provider@credentials(), "api-key")
  req <- req_url_query(req, `api-version` = provider@api_version)

  if (length(provider@beta_headers) > 0) {
    req <- req_headers(req, `anthropic-beta` = provider@beta_headers)
  }

  req <- req_error(req, body = function(resp) {
    if (resp_content_type(resp) == "application/json") {
      json <- resp_body_json(resp)
      paste0(json$error$message, " [", json$error$type, "]")
    }
  })

  req
}

default_azure_anthropic_credentials <- function() {
  azure_scope <- "https://cognitiveservices.azure.com/.default"

  # Detect viewer-based credentials from Posit Connect.
  if (has_connect_viewer_token(scope = azure_scope)) {
    return(function() {
      token <- connectcreds::connect_viewer_token(scope = azure_scope)
      list(Authorization = paste("Bearer", token$access_token))
    })
  }

  # Detect Azure service principals.
  tenant_id <- Sys.getenv("AZURE_TENANT_ID")
  client_id <- Sys.getenv("AZURE_CLIENT_ID")
  client_secret <- Sys.getenv("AZURE_CLIENT_SECRET")
  if (nchar(tenant_id) && nchar(client_id) && nchar(client_secret)) {
    client <- oauth_client(
      client_id,
      token_url = paste0(
        "https://login.microsoftonline.com/",
        tenant_id,
        "/oauth2/v2.0/token"
      ),
      secret = client_secret,
      auth = "body",
      name = "ellmer-azure-anthropic-sp"
    )
    return(function() {
      token <- oauth_token_cached(
        client,
        oauth_flow_client_credentials,
        flow_params = list(scope = azure_scope),
        reauth = is_testing()
      )
      list(Authorization = paste("Bearer", token$access_token))
    })
  }

  # If we have an API key, include it in the credentials.
  api_key <- Sys.getenv("AZURE_ANTHROPIC_API_KEY")
  if (nchar(api_key)) {
    return(\() api_key)
  }

  # Masquerade as the Azure CLI.
  client_id <- "04b07795-8ddb-461a-bbee-02f9e1bf7b46"
  if (is_interactive() && !is_hosted_session()) {
    client <- oauth_client(
      client_id,
      token_url = "https://login.microsoftonline.com/common/oauth2/v2.0/token",
      secret = "",
      auth = "body",
      name = paste0("ellmer-", client_id)
    )
    return(function() {
      token <- oauth_token_cached(
        client,
        oauth_flow_auth_code,
        flow_params = list(
          auth_url = "https://login.microsoftonline.com/common/oauth2/v2.0/authorize",
          scope = paste(azure_scope, "offline_access"),
          redirect_uri = "http://localhost:8400",
          auth_params = list(prompt = "select_account")
        )
      )
      list(Authorization = paste("Bearer", token$access_token))
    })
  }

  if (is_testing()) {
    testthat::skip("no Azure credentials available")
  }

  cli::cli_abort("No Azure credentials are available.")
}
