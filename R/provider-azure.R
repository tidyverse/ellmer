#' @include provider-openai-compatible.R
#' @include content.R
NULL

# https://learn.microsoft.com/en-us/azure/ai-services/openai/reference#chat-completions

#' Chat with a model hosted on Azure OpenAI
#'
#' @description
#' The [Azure OpenAI server](https://azure.microsoft.com/en-us/products/ai-services/openai-service)
#' hosts a number of open source models as well as proprietary models
#' from OpenAI.
#'
#' Built on top of [chat_openai_compatible()].
#'
#' ## Authentication
#'
#' `chat_azure_openai()` supports API keys and the `credentials` parameter, but
#' it also makes use of:
#'
#' - Azure service principals (when the `AZURE_TENANT_ID`, `AZURE_CLIENT_ID`,
#'   and `AZURE_CLIENT_SECRET` environment variables are set).
#' - Interactive Entra ID authentication, like the Azure CLI.
#' - Viewer-based credentials on Posit Connect. Requires the \pkg{connectcreds}
#'   package.
#'
#' @param endpoint Azure OpenAI endpoint url with protocol and hostname, i.e.
#'  `https://{your-resource-name}.openai.azure.com`. Defaults to using the
#'   value of the `AZURE_OPENAI_ENDPOINT` environment variable.
#' @param model The **deployment id** for the model you want to use.
#' @param deployment_id `r lifecycle::badge("deprecated")` Use `model` instead.
#' @param api_version The API version to use.
#' @param api_key `r lifecycle::badge("deprecated")` Use `credentials` instead.
#' @param credentials `r api_key_param("AZURE_OPENAI_API_KEY")`
#' @inheritParams chat_openai
#' @inherit chat_openai return
#' @family chatbots
#' @export
#' @examples
#' \dontrun{
#' chat <- chat_azure_openai(model = "gpt-4o-mini")
#' chat$chat("Tell me three jokes about statisticians")
#' }
chat_azure_openai <- function(
  endpoint = azure_endpoint(),
  model,
  params = NULL,
  api_version = NULL,
  system_prompt = NULL,
  api_key = NULL,
  credentials = NULL,
  api_args = list(),
  echo = c("none", "output", "all"),
  api_headers = character(),
  deployment_id = deprecated()
) {
  if (lifecycle::is_present(deployment_id)) {
    lifecycle::deprecate_warn(
      when = "0.4.0",
      what = "chat_azure_openai(deployment_id=)",
      with = "chat_azure_openai(model=)",
    )
    model <- deployment_id
  }
  check_string(endpoint)
  check_string(model)
  params <- params %||% params()
  api_version <- set_default(api_version, "2024-10-21")

  credentials <- as_credentials(
    "chat_azure_openai",
    default_azure_credentials(),
    credentials = credentials,
    api_key = api_key
  )

  echo <- check_echo(echo)

  provider <- ProviderAzureOpenAI(
    name = "Azure/OpenAI",
    base_url = paste0(endpoint, "/openai/deployments/", model),
    model = model,
    params = params,
    api_version = api_version,
    credentials = credentials,
    extra_args = api_args,
    extra_headers = api_headers
  )
  Chat$new(provider = provider, system_prompt = system_prompt, echo = echo)
}

chat_azure_openai_test <- function(
  system_prompt = NULL,
  params = NULL,
  ...,
  echo = "none"
) {
  credentials <- \() key_get("AZURE_OPENAI_API_KEY")
  default_params <- params(seed = 1014, temperature = 0)
  params <- modify_list(default_params, params %||% params())

  chat_azure_openai(
    ...,
    system_prompt = system_prompt,
    credentials = credentials,
    endpoint = "https://ai-hwickhamai260967855527.openai.azure.com",
    model = "gpt-4o-mini",
    params = params,
    echo = echo
  )
}

ProviderAzureOpenAI <- new_class(
  "ProviderAzureOpenAI",
  parent = ProviderOpenAICompatible,
  properties = list(
    api_version = prop_string()
  )
)

# https://learn.microsoft.com/en-us/azure/ai-services/openai/how-to/switching-endpoints#api-key
azure_endpoint <- function() {
  key_get("AZURE_OPENAI_ENDPOINT")
}

# https://learn.microsoft.com/en-us/azure/ai-services/openai/reference#chat-completions
method(base_request, ProviderAzureOpenAI) <- function(provider) {
  req <- request(provider@base_url)
  req <- ellmer_req_robustify(req)
  req <- ellmer_req_user_agent(req)
  req <- base_request_error(provider, req)

  req <- req_url_query(req, `api-version` = provider@api_version)
  req <- ellmer_req_credentials(req, provider@credentials(), "api-key")
  req
}

method(base_request_error, ProviderAzureOpenAI) <- function(provider, req) {
  req_error(req, body = function(resp) {
    error <- resp_body_json(resp)$error
    msg <- paste0(error$code, ": ", error$message)
    # Try to be helpful in the (common) case that the user or service
    # principal is missing the necessary role.
    # See: https://learn.microsoft.com/en-us/azure/ai-services/openai/how-to/role-based-access-control
    bad_rbac <- identical(
      error$message,
      "Principal does not have access to API/Operation."
    )
    if (bad_rbac) {
      msg <- c(
        "*" = msg,
        "i" = cli::format_inline(
          "Your user or service principal likely needs one of the following
        roles: {.emph Cognitive Services OpenAI User},
        {.emph Cognitive Services OpenAI Contributor}, or
        {.emph Cognitive Services Contributor}.",
          keep_whitespace = FALSE
        )
      )
    }
    msg
  })
}

default_azure_credentials <- function() {
  azure_openai_scope <- "https://cognitiveservices.azure.com/.default"

  # Detect viewer-based credentials from Posit Connect.
  if (has_connect_viewer_token(scope = azure_openai_scope)) {
    return(function() {
      token <- connectcreds::connect_viewer_token(scope = azure_openai_scope)
      list(Authorization = paste("Bearer", token$access_token))
    })
  }

  # Detect Azure service principals.
  tenant_id <- Sys.getenv("AZURE_TENANT_ID")
  client_id <- Sys.getenv("AZURE_CLIENT_ID")
  client_secret <- Sys.getenv("AZURE_CLIENT_SECRET")
  if (nchar(tenant_id) && nchar(client_id) && nchar(client_secret)) {
    # Service principals use an OAuth client credentials flow. We cache the token
    # so we don't need to perform this flow before each turn.
    client <- oauth_client(
      client_id,
      token_url = paste0(
        "https://login.microsoftonline.com/",
        tenant_id,
        "/oauth2/v2.0/token"
      ),
      secret = client_secret,
      auth = "body",
      name = "ellmer-azure-sp"
    )
    return(function() {
      token <- oauth_token_cached(
        client,
        oauth_flow_client_credentials,
        flow_params = list(scope = azure_openai_scope),
        # Don't use the cached token when testing.
        reauth = is_testing()
      )
      list(Authorization = paste("Bearer", token$access_token))
    })
  }

  # If we have an API key, include it in the credentials.
  api_key <- Sys.getenv("AZURE_OPENAI_API_KEY")
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
          scope = paste(azure_openai_scope, "offline_access"),
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
