#' @include provider-openai.R
NULL

#' Chat with a model hosted on perplexity.ai
#'
#' @description
#' Sign up at <https://www.perplexity.ai>.
#'
#' Perplexity AI is a platform for running LLMs that are capable of
#' searching the web in real-time to help them answer questions with
#' information that may not have been available when the model was
#' trained.
#'
#' This function is a lightweight wrapper around [chat_openai()] with
#' the defaults tweaked for Perplexity AI.
#'
#' @export
#' @family chatbots
#' @param api_key `r lifecycle::badge("deprecated")` Use `credentials` instead.
#' @param credentials `r api_key_param("PERPLEXITY_API_KEY")`
#' @param model `r param_model("llama-3.1-sonar-small-128k-online")`
#' @param params Common model parameters, usually created by [params()].
#' @inheritParams chat_openai
#' @inherit chat_openai return
#' @examples
#' \dontrun{
#' chat <- chat_perplexity()
#' chat$chat("Tell me three jokes about statisticians")
#' }
chat_perplexity <- function(
  system_prompt = NULL,
  base_url = "https://api.perplexity.ai/",
  api_key = NULL,
  credentials = NULL,
  model = NULL,
  params = NULL,
  api_args = list(),
  echo = NULL,
  api_headers = character()
) {
  model <- set_default(model, "llama-3.1-sonar-small-128k-online")
  echo <- check_echo(echo)

  credentials <- as_credentials(
    "chat_perplexity",
    function() perplexity_key(),
    credentials = credentials,
    api_key = api_key
  )

  params <- params %||% params()

  provider <- ProviderPerplexity(
    name = "Perplexity",
    base_url = base_url,
    model = model,
    params = params,
    extra_args = api_args,
    credentials = credentials,
    extra_headers = api_headers
  )
  Chat$new(provider = provider, system_prompt = system_prompt, echo = echo)
}

ProviderPerplexity <- new_class(
  "ProviderPerplexity",
  parent = ProviderOpenAI,
)

method(chat_params, ProviderPerplexity) <- function(provider, params) {
  # https://docs.perplexity.ai/api-reference/chat-completions-post
  standardise_params(
    params,
    c(
      max_tokens = "max_tokens",
      temperature = "temperature",
      top_p = "top_p",
      top_k = "top_k",
      presence_penalty = "presence_penalty",
      frequency_penalty = "frequency_penalty"
    )
  )
}

perplexity_key <- function() {
  key_get("PERPLEXITY_API_KEY")
}
