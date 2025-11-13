#' @include provider-openai-compatible.R
NULL

#' Chat with a model hosted on DeepSeek
#'
#' @description
#' Sign up at <https://platform.deepseek.com>.
#'
#' Built on top of [chat_openai_compatible()].
#'
#' ## Known limitations
#'
#' * Structured data extraction is not supported.
#' * Images are not supported.
#'
#' @export
#' @family chatbots
#' @inheritParams chat_openai
#' @param api_key `r lifecycle::badge("deprecated")` Use `credentials` instead.
#' @param credentials `r api_key_param("DEEPSEEK_API_KEY")`
#' @param base_url The base URL to the endpoint; the default uses DeepSeek.
#' @param model `r param_model("deepseek-chat")`
#' @inherit chat_openai return
#' @examples
#' \dontrun{
#' chat <- chat_deepseek()
#' chat$chat("Tell me three jokes about statisticians")
#' }
chat_deepseek <- function(
  system_prompt = NULL,
  base_url = "https://api.deepseek.com",
  api_key = NULL,
  credentials = NULL,
  model = NULL,
  params = NULL,
  api_args = list(),
  echo = NULL,
  api_headers = character()
) {
  model <- set_default(model, "deepseek-chat")
  echo <- check_echo(echo)

  credentials <- as_credentials(
    "chat_deepseek",
    function() deepseek_key(),
    credentials = credentials,
    api_key = api_key
  )

  params <- params %||% params()

  provider <- ProviderDeepSeek(
    name = "DeepSeek",
    base_url = base_url,
    model = model,
    params = params,
    extra_args = api_args,
    credentials = credentials,
    extra_headers = api_headers
  )
  Chat$new(provider = provider, system_prompt = system_prompt, echo = echo)
}

ProviderDeepSeek <- new_class(
  "ProviderDeepSeek",
  parent = ProviderOpenAICompatible
)

method(chat_params, ProviderDeepSeek) <- function(provider, params) {
  # https://platform.deepseek.com/api-docs/api/create-chat-completion
  standardise_params(
    params,
    c(
      frequency_penalty = "frequency_penalty",
      max_tokens = "max_tokens",
      presence_penalty = "presence_penalty",
      stop = "stop_sequences",
      temperature = "temperature",
      top_p = "top_p",
      logprobs = "log_probs",
      top_logprobs = "top_k"
    )
  )
}

method(as_json, list(ProviderDeepSeek, ContentText)) <- function(
  provider,
  x,
  ...
) {
  x@text
}

method(as_json, list(ProviderDeepSeek, Turn)) <- function(provider, x, ...) {
  if (is_user_turn(x)) {
    x <- turn_contents_expand(x)
    data <- turn_split_tool_results(x)

    # Text and tool results go in separate messages
    texts <- keep(data$contents, S7_inherits, ContentText)
    texts_out <- lapply(texts, function(text) {
      list(role = "user", content = as_json(provider, text, ...))
    })

    tools_out <- lapply(data$tool_results, function(tool) {
      list(
        role = "tool",
        content = tool_string(tool),
        tool_call_id = tool@request@id
      )
    })

    c(tools_out, texts_out)
  } else if (is_assistant_turn(x)) {
    # Tool requests come out of content and go into own argument
    text <- detect(x@contents, S7_inherits, ContentText)
    tools <- keep(x@contents, is_tool_request)

    list(compact(list(
      role = "assistant",
      content = as_json(provider, text, ...),
      tool_calls = as_json(provider, tools, ...)
    )))
  } else {
    as_json(super(provider, ProviderOpenAICompatible), x, ...)
  }
}

deepseek_key <- function() key_get("DEEPSEEK_API_KEY")
