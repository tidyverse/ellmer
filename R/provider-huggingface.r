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
#' * Parameter support is hit or miss.
#' * Tool calling is currently broken in the API.
#' * While images are technically supported, I couldn't find any models that
#'   returned useful respones.
#' * Some models do not support the chat interface or parts of it, for example
#'   `google/gemma-2-2b-it` does not support a system prompt. You will need to
#'   carefully choose the model.
#'
#' So overall, not something we could recommend at the moment.
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
  echo = NULL
) {
  model <- set_default(model, "meta-llama/Llama-3.1-8B-Instruct")
  echo <- check_echo(echo)
  params <- params %||% params()

  base_url <- paste0(
    "https://api-inference.huggingface.co/models/",
    model,
    "/v1"
  )

  provider <- ProviderHuggingFace(
    name = "HuggingFace",
    base_url = base_url,
    model = model,
    params = params,
    extra_args = api_args,
    api_key = api_key
  )
  Chat$new(provider = provider, system_prompt = system_prompt, echo = echo)
}

ProviderHuggingFace <- new_class("ProviderHuggingFace", parent = ProviderOpenAI)

chat_huggingface_test <- function(..., model = NULL) {
  model <- model %||% "meta-llama/Llama-3.1-8B-Instruct"
  chat_huggingface(model = model, ...)
}

# https://platform.openai.com/docs/api-reference/chat/create
method(chat_body, ProviderHuggingFace) <- function(
  provider,
  stream = TRUE,
  turns = list(),
  tools = list(),
  type = NULL
) {
  if (length(tools) > 0) {
    # https://github.com/huggingface/text-generation-inference/issues/2986
    cli::cli_abort("HuggingFace does not currently support tools.")
  }

  body <- chat_body(
    super(provider, ProviderOpenAI),
    stream = stream,
    turns = turns,
    tools = tools,
    type = type
  )

  messages <- compact(unlist(as_json(provider, turns), recursive = FALSE))
  tools <- as_json(provider, unname(tools))

  if (!is.null(type)) {
    body$response_format <- list(
      type = "json",
      value = as_json(provider, type)
    )
  }

  body
}

method(as_json, list(ProviderHuggingFace, ContentToolResult)) <- function(
  provider,
  x
) {
  list(
    role = "tool",
    content = tool_string(x),
    name = x@request@name,
    tool_call_id = x@request@id
  )
}

hf_key <- function() {
  key_get("HUGGINGFACE_API_KEY")
}
