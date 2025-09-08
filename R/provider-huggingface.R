#' Chat with a model hosted on Hugging Face Serverless Inference API
#'
#' @description
#' [Hugging Face](https://huggingface.co/) hosts a variety of open-source
#' and proprietary AI models available via their Inference API.
#' To use the Hugging Face API, you must have an Access Token, which you can obtain
#' from your [Hugging Face account](https://huggingface.co/settings/tokens)
#' (ensure that at least "Make calls to Inference Providers" and
#' "Make calls to your Inference Endpoints" is checked).
#'
#' This function is a lightweight wrapper around [chat_openai()], with
#' the defaults adjusted for Hugging Face.
#'
#' ## Known limitations
#'
#' * Some models do not support the chat interface or parts of it, for example
#'   `google/gemma-2-2b-it` does not support a system prompt. You will need to
#'   carefully choose the model.
#'
#' @family chatbots
#' @param model `r param_model("meta-llama/Llama-3.1-8B-Instruct")`
#' @param api_key The API key to use for authentication. You generally should
#'   not supply this directly, but instead set the `HUGGINGFACE_API_KEY` environment
#'   variable.
#' @export
#' @inheritParams chat_openai
#' @inherit chat_openai return
#' @examples
#' \dontrun{
#' chat <- chat_huggingface()
#' chat$chat("Tell me three jokes about statisticians")
#' }
chat_huggingface <- function(
  system_prompt = NULL,
  params = NULL,
  api_key = hf_key(),
  model = NULL,
  api_args = list(),
  echo = NULL,
  api_headers = character()
) {
  model <- set_default(model, "meta-llama/Llama-3.1-8B-Instruct")
  echo <- check_echo(echo)
  params <- params %||% params()

  # https://huggingface.co/docs/inference-providers/en/index?python-clients=requests#http--curl
  base_url <- "https://router.huggingface.co/v1/"

  provider <- ProviderHuggingFace(
    name = "HuggingFace",
    base_url = base_url,
    model = model,
    params = params,
    extra_args = api_args,
    api_key = api_key,
    extra_headers = api_headers
  )
  Chat$new(provider = provider, system_prompt = system_prompt, echo = echo)
}

ProviderHuggingFace <- new_class("ProviderHuggingFace", parent = ProviderOpenAI)

chat_huggingface_test <- function(
  system_prompt = "Be terse",
  ...,
  model = NULL,
  echo = "none"
) {
  model <- model %||% "Qwen/Qwen3-4B"
  chat_huggingface(model = model, ..., echo = echo)
}

hf_key <- function() {
  key_get("HUGGINGFACE_API_KEY")
}
