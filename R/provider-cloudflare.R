#' @include provider-gemini.R
NULL

#' Chat with a model hosted on CloudFlare
#'
#' @description
#' [Cloudflare](https://www.cloudflare.com/developer-platform/products/workers-ai/)
#' works AI hosts a variety of open-source AI models. To use the Cloudflare
#' API, you must have an Account ID and an Access Token, which you can obtain
#' [by following these instructions](https://developers.cloudflare.com/workers-ai/get-started/rest-api/).
#'
#' ## Known limitations
#' * Tool calling does not appear to work.
#' * Images don't appear to work.
#'
#' @family chatbots
#' @param model `r param_model("meta-llama/Llama-3.3-70b-instruct-fp8-fast")`
#' @param api_key `r api_key_param("CLOUDFLARE_API_KEY")`
#' @param account The Cloudflare account ID. Taken from the
#'   `CLOUDFLARE_ACCOUNT_ID` env var, if defined.
#' @param api_key The API key to use for authentication. You generally should
#'   not supply this directly, but instead set the `HUGGINGFACE_API_KEY` environment
#'   variable.
#' @export
#' @inheritParams chat_openai
#' @inherit chat_openai return
#' @examples
#' \dontrun{
#' chat <- chat_cloudflare()
#' chat$chat("Tell me three jokes about statisticians")
#' }
chat_cloudflare <- function(
  account = cloudflare_account(),
  system_prompt = NULL,
  params = NULL,
  api_key = cloudflare_key(),
  model = NULL,
  api_args = list(),
  echo = NULL
) {
  # List at https://developers.cloudflare.com/workers-ai/models/
  # `@cf` appears to be part of the model name
  model <- set_default(model, "@cf/meta/llama-3.3-70b-instruct-fp8-fast")
  echo <- check_echo(echo)
  params <- params %||% params()

  # https://developers.cloudflare.com/workers-ai/configuration/open-ai-compatibility/
  cloudflare_api <- "https://api.cloudflare.com/client/v4/accounts/"
  base_url <- paste0(cloudflare_api, cloudflare_account(), "/ai/v1/")

  provider <- ProviderCloudflare(
    name = "Cloudflare",
    base_url = base_url,
    model = model,
    params = params,
    api_key = api_key,
    extra_args = api_args
  )

  Chat$new(provider = provider, system_prompt = system_prompt, echo = echo)
}

ProviderCloudflare <- new_class("ProviderCloudflare", parent = ProviderOpenAI)

method(base_request_error, ProviderCloudflare) <- function(provider, req) {
  req_error(req, body = function(resp) {
    if (resp_content_type(resp) == "application/json") {
      resp_body_json(resp)$errors[[1]]$message
    } else if (resp_content_type(resp) == "text/plain") {
      resp_body_string(resp)
    }
  })
}


# Docs look like Gemini tool defs
# https://developers.cloudflare.com/workers-ai/features/function-calling/traditional/
method(as_json, list(ProviderCloudflare, ToolDef)) <-
  method(as_json, list(ProviderGoogleGemini, ToolDef))

method(as_json, list(ProviderCloudflare, TypeObject)) <-
  method(as_json, list(ProviderGoogleGemini, TypeObject))


chat_cloudflare_test <- function(..., model = NULL, echo = "none") {
  model <- model %||% "@cf/meta/llama-3.3-70b-instruct-fp8-fast"
  chat_cloudflare(model = model, ..., echo = echo)
}

cloudflare_key <- function() {
  key_get("CLOUDFLARE_API_KEY")
}

cloudflare_account <- function() {
  key_get("CLOUDFLARE_ACCOUNT_ID")
}
