#' @include provider-openai.R
NULL

#' Chat with a model hosted on DeepSeek
#'
#' @description
#' Sign up at <https://platform.deepseek.com>.
#'
#' ## Known limitations
#'
#' * Structured data extraction is not supported.
#' * Images are not supported.
#'
#' @export
#' @family chatbots
#' @inheritParams chat_openai
#' @param api_key `r api_key_param("DEEPSEEK_API_KEY")`
#' @param base_url The base URL to the endpoint; the default uses DeepSeek.
#' @inherit chat_openai return
#' @examples
#' \dontrun{
#' chat <- chat_deepseek()
#' chat$chat("Tell me three jokes about statisticians")
#' }
chat_deepseek <- function(
  system_prompt = NULL,
  turns = NULL,
  base_url = "https://api.deepseek.com",
  api_key = deepseek_key(),
  model = NULL,
  seed = NULL,
  api_args = list(),
  echo = NULL
) {
  turns <- normalize_turns(turns, system_prompt)
  model <- set_default(model, "deepseek-chat")
  echo <- check_echo(echo)

  if (is_testing() && is.null(seed)) {
    seed <- seed %||% 1014
  }

  provider <- ProviderDeepSeek(
    name = "DeepSeek",
    base_url = base_url,
    model = model,
    seed = seed,
    extra_args = api_args,
    api_key = api_key
  )
  Chat$new(provider = provider, turns = turns, echo = echo)
}

ProviderDeepSeek <- new_class("ProviderDeepSeek", parent = ProviderOpenAI)

method(as_json, list(ProviderDeepSeek, ContentText)) <- function(provider, x) {
  x@text
}

method(as_json, list(ProviderDeepSeek, Turn)) <- function(provider, x) {
  if (x@role == "user") {
    # Text and tool results go in separate messages
    texts <- keep(x@contents, S7_inherits, ContentText)
    texts_out <- lapply(texts, function(text) {
      list(role = "user", content = as_json(provider, text))
    })

    tools <- keep(x@contents, S7_inherits, ContentToolResult)
    tools_out <- lapply(tools, function(tool) {
      list(role = "tool", content = tool_string(tool), tool_call_id = tool@id)
    })

    c(texts_out, tools_out)
  } else if (x@role == "assistant") {
    # Tool requests come out of content and go into own argument
    text <- detect(x@contents, S7_inherits, ContentText)
    tools <- keep(x@contents, S7_inherits, ContentToolRequest)

    list(compact(list(
      role = "assistant",
      content = as_json(provider, text),
      tool_calls = as_json(provider, tools)
    )))
  } else {
    as_json(super(provider, ProviderOpenAI), x)
  }
}

deepseek_key <- function() key_get("DEEPSEEK_API_KEY")
