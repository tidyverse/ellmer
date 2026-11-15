#' @include provider-openai-compatible.R
NULL

#' Chat with a model hosted on DeepSeek
#'
#' @description
#' `r support_badge("official")`
#'
#' Sign up at <https://platform.deepseek.com>.
#'
#' Built on top of [chat_openai_compatible()].
#'
#' ## Thinking
#'
#' Set `reasoning_effort` in [params()] to `"low"`, `"high"`, or `"max"` to
#' control thinking effort. Use `params(reasoning_effort = "none")` to disable
#' thinking. When omitted, DeepSeek enables thinking with `"high"` effort by
#' default.
#'
#' You can also explicitly enable or disable thinking with
#' `api_args = list(thinking = list(type = "enabled"))` or `type = "disabled"`.
#' This setting takes precedence over `reasoning_effort` for selecting the mode.
#'
#' ellmer sends previous assistant thinking back as `reasoning_content`, which
#' DeepSeek requires for thinking conversations with tools. DeepSeek ignores
#' this content when tools are absent. See the
#' [DeepSeek thinking guide](https://api-docs.deepseek.com/guides/thinking_mode/)
#' for details.
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
#' @param model `r param_model("deepseek-flash")`
#' @inherit chat_openai return
#' @examples
#' \dontrun{
#' chat <- chat_deepseek()
#' chat$chat("Tell me three jokes about statisticians")
#'
#' chat <- chat_deepseek(
#'   model = "deepseek-flash",
#'   params = params(reasoning_effort = "max")
#' )
#' chat$chat("What is the smallest prime greater than 100?")
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
  model <- set_default(model, "deepseek-flash")
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
    credentials = credentials,
    extra_headers = api_headers,
    strict = FALSE
  )
  model <- Model(name = model, params = params, extra_args = api_args)
  Chat$new(
    provider = provider,
    model = model,
    system_prompt = system_prompt,
    echo = echo
  )
}

ProviderDeepSeek <- new_class(
  "ProviderDeepSeek",
  parent = ProviderOpenAICompatible
)

method(chat_params, ProviderDeepSeek) <- function(provider, params) {
  # https://api-docs.deepseek.com/api/create-chat-completion/
  standardise_params(
    params,
    c(
      frequency_penalty = "frequency_penalty",
      max_tokens = "max_tokens",
      presence_penalty = "presence_penalty",
      reasoning_effort = "reasoning_effort",
      stop = "stop_sequences",
      temperature = "temperature",
      top_p = "top_p",
      logprobs = "log_probs"
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
    thinking <- keep(x@contents, S7_inherits, ContentThinking)

    reasoning_content <- if (length(thinking) > 0) {
      paste0(map_chr(thinking, function(x) x@thinking), collapse = "")
    }

    content <- if (!is.null(text)) as_json(provider, text, ...)
    list(compact(list(
      role = "assistant",
      content = content,
      tool_calls = as_json(provider, tools, ...),
      reasoning_content = reasoning_content
    )))
  } else {
    as_json(super(provider, ProviderOpenAICompatible), x, ...)
  }
}

deepseek_key <- function() key_get("DEEPSEEK_API_KEY")

#' @export
#' @rdname chat_deepseek
models_deepseek <- function(
  base_url = "https://api.deepseek.com",
  api_key = NULL,
  credentials = NULL
) {
  credentials <- as_credentials(
    "models_deepseek",
    \() deepseek_key(),
    credentials = credentials,
    api_key = api_key
  )

  provider <- ProviderDeepSeek(
    name = "DeepSeek",
    base_url = base_url,
    credentials = credentials
  )

  models_list(provider)
}

method(models_list, ProviderDeepSeek) <- function(provider) {
  req <- base_request(provider)
  req <- req_url_path_append(req, "/models")
  resp <- req_perform(req)

  json <- resp_body_json(resp)

  id <- map_chr(json$data, "[[", "id")
  owned_by <- map_chr(json$data, "[[", "owned_by")

  df <- data.frame(id = id, owned_by = owned_by)
  cbind(df, match_prices(provider@name, df$id))
}
