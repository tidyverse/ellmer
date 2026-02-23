#' @include provider-openai-compatible.R
NULL

#' Chat with a model hosted on Groq
#'
#' @description
#' Sign up at <https://groq.com>.
#'
#' Built on top of [chat_openai_compatible()].
#'
#' @export
#' @family chatbots
#' @param api_key `r lifecycle::badge("deprecated")` Use `credentials` instead.
#' @param credentials `r api_key_param("GROQ_API_KEY")`
#' @param model `r param_model("llama-3.1-8b-instant")`
#' @param params Common model parameters, usually created by [params()].
#' @inheritParams chat_openai
#' @inherit chat_openai return
#' @examples
#' \dontrun{
#' chat <- chat_groq()
#' chat$chat("Tell me three jokes about statisticians")
#' }
chat_groq <- function(
  system_prompt = NULL,
  base_url = "https://api.groq.com/openai/v1",
  api_key = NULL,
  credentials = NULL,
  model = NULL,
  params = NULL,
  api_args = list(),
  echo = NULL,
  api_headers = character()
) {
  model <- set_default(model, "llama-3.1-8b-instant")
  echo <- check_echo(echo)

  credentials <- as_credentials(
    "chat_groq",
    function() groq_key(),
    credentials = credentials,
    api_key = api_key
  )

  # https://console.groq.com/docs/api-reference#chat-create (same as OpenAI)
  params <- params %||% params()

  provider <- ProviderGroq(
    name = "Groq",
    base_url = base_url,
    model = model,
    params = params,
    extra_args = api_args,
    credentials = credentials,
    extra_headers = api_headers
  )
  Chat$new(provider = provider, system_prompt = system_prompt, echo = echo)
}

ProviderGroq <- new_class("ProviderGroq", parent = ProviderOpenAICompatible)


method(as_json, list(ProviderGroq, ToolDef)) <- function(provider, x, ...) {
  list(
    type = "function",
    "function" = compact(list(
      name = x@name,
      description = x@description,
      parameters = as_json(provider, x@arguments, ...)
    ))
  )
}

groq_key <- function() {
  key_get("GROQ_API_KEY")
}
