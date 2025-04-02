#' Chat with a model hosted on Hugging Face Serverless Inference API
#'
#' @description
#' [Hugging Face](https://huggingface.co/) hosts a variety of open-source
#' and proprietary AI models available via their Inference API.
#' To use the Hugging Face API, you must have an Access Token, which you can obtain
#' from your [Hugging Face account](https://huggingface.co/settings/tokens).
#'
#' This function is a lightweight wrapper around [chat_openai()], with
#' the defaults adjusted for Hugging Face. Model defaults to `meta-llama/Llama-3.1-8B-Instruct`.
#'
#' ## Known limitations
#'
#' * Some models do not support the chat interface or parts of it, for example
#'   `google/gemma-2-2b-it` does not support a system prompt. You will need to
#'   carefully choose the model.
#'
#' @family chatbots
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
  turns = NULL,
  base_url = "https://api-inference.huggingface.co/models/",
  api_key = hf_key(),
  model = NULL,
  seed = NULL,
  api_args = list(),
  echo = NULL
) {
  model <- set_default(model, "meta-llama/Llama-3.1-8B-Instruct")
  echo <- check_echo(echo)

  chat_openai(
    system_prompt = system_prompt,
    turns = turns,
    # modify base_url for hugging face compatibility with openai
    base_url = paste0(base_url, model, "/v1"),
    api_key = api_key,
    model = model,
    seed = seed,
    api_args = api_args,
    echo = echo
  )
}

hf_key <- function() {
  key_get("HUGGINGFACE_API_KEY")
}
