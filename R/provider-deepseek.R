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
#' DeepSeek models think by default. Use `reasoning_effort` in [params()] to
#' control this: `"low"`, `"high"` (the default), or `"max"`, or `"none"` to
#' turn thinking off. `"medium"` is treated as `"high"`.
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
    preserve_thinking = TRUE,
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

  df <- data.frame(
    id = id,
    owned_by = owned_by,
    max_context_length = map_token_limit(json$data, "context_window")
  )
  cbind(df, match_prices(provider@name, df$id))
}
