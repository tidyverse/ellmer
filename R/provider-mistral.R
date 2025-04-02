#' Chat with a model hosted on Mistral's La Platforme
#'
#' @description
#' Get your API key from <https://console.mistral.ai/api-keys>.
#'
#' ## Known limitations
#'
#' * Tool calling is unstable.
#' * Images require a model that supports images.
#'
#' @export
#' @family chatbots
#' @param api_key `r api_key_param("MISTRAL_API_KEY")`
#' @inheritParams chat_openai
#' @inherit chat_openai return
#' @examples
#' \dontrun{
#' chat <- chat_mistral()
#' chat$chat("Tell me three jokes about statisticians")
#' }
chat_mistral <- function(
  system_prompt = NULL,
  turns = NULL,
  params = NULL,
  api_key = mistral_key(),
  model = NULL,
  seed = NULL,
  api_args = list(),
  echo = NULL
) {
  turns <- normalize_turns(turns, system_prompt)
  params <- params %||% params()
  model <- set_default(model, "mistral-large-latest")
  echo <- check_echo(echo)

  provider <- ProviderMistral(
    base_url = "https://api.mistral.ai/v1/",
    model = model,
    params = params,
    seed = seed,
    extra_args = api_args,
    api_key = api_key
  )
  Chat$new(provider = provider, turns = turns, echo = echo)
}

ProviderMistral <- new_class("ProviderMistral", parent = ProviderOpenAI)

chat_mistral_test <- function(model = NULL, params = NULL, ...) {
  model <- model %||% "mistral-large-latest"

  params <- params %||% params()
  params <- modify_list(list(seed = 1014, temperature = 0), params)

  chat_mistral(
    model = model,
    params = params,
    ...
  )
}

method(chat_request, ProviderMistral) <- function(
  provider,
  stream = TRUE,
  turns = list(),
  tools = list(),
  type = NULL
) {
  req <- request(provider@base_url)
  req <- req_url_path_append(req, "/chat/completions")
  req <- req_auth_bearer_token(req, provider@api_key)
  req <- req_retry(req, max_tries = 2)
  req <- ellmer_req_timeout(req, stream)
  req <- ellmer_req_user_agent(req)

  req <- req_error(req, body = function(resp) {
    if (resp_content_type(resp) == "application/json") {
      resp_body_json(resp)$error$message
    } else if (resp_content_type(resp) == "text/plain") {
      resp_body_string(resp)
    }
  })

  messages <- compact(unlist(as_json(provider, turns), recursive = FALSE))
  tools <- as_json(provider, unname(tools))

  if (!is.null(type)) {
    response_format <- list(
      type = "json_schema",
      json_schema = list(
        name = "structured_data",
        schema = as_json(provider, type),
        strict = TRUE
      )
    )
  } else {
    response_format <- NULL
  }

  params <- chat_params(provider, provider@params)

  body <- compact(list2(
    messages = messages,
    model = provider@model,
    !!!params,
    stream = stream,
    # TODO: rewrite with chat_body when it exists
    # stream_options = if (stream) list(include_usage = TRUE),
    tools = tools,
    response_format = response_format
  ))
  body <- utils::modifyList(body, provider@extra_args)
  req <- req_body_json(req, body)

  req
}


method(chat_params, ProviderMistral) <- function(provider, params) {
  standardise_params(
    params,
    c(
      temperature = "temperature",
      top_p = "top_p",
      frequency_penalty = "frequency_penalty",
      presence_penalty = "presence_penalty",
      random_seed = "seed",
      max_tokens = "max_tokens",
      logprobs = "log_probs",
      stop = "stop_sequences"
    )
  )
}


mistral_key <- function() {
  key_get("MISTRAL_API_KEY")
}
