#' Chat with a model hosted on Mistral's La Platforme
#'
#' @description
#' Get your API key from <https://console.mistral.ai/api-keys>.
#'
#' Built on top of [chat_openai_compatible()].
#'
#' ## Known limitations
#'
#' * Tool calling is unstable.
#' * Images require a model that supports images.
#'
#' @export
#' @family chatbots
#' @param model `r param_model("mistral-large-latest")`
#' @param api_key `r lifecycle::badge("deprecated")` Use `credentials` instead.
#' @param credentials `r api_key_param("MISTRAL_API_KEY")`
#' @inheritParams chat_openai
#' @inherit chat_openai return
#' @examples
#' \dontrun{
#' chat <- chat_mistral()
#' chat$chat("Tell me three jokes about statisticians")
#' }
chat_mistral <- function(
  system_prompt = NULL,
  params = NULL,
  api_key = NULL,
  credentials = NULL,
  model = NULL,
  api_args = list(),
  echo = NULL,
  api_headers = character()
) {
  params <- params %||% params()
  model <- set_default(model, "mistral-large-latest")
  echo <- check_echo(echo)

  credentials <- as_credentials(
    "chat_mistral",
    function() mistral_key(),
    credentials = credentials,
    api_key = api_key
  )

  provider <- ProviderMistral(
    name = "Mistral",
    base_url = mistral_base_url,
    model = model,
    params = params,
    extra_args = api_args,
    credentials = credentials,
    extra_headers = api_headers
  )
  Chat$new(provider = provider, system_prompt = system_prompt, echo = echo)
}

mistral_base_url <- "https://api.mistral.ai/v1/"
ProviderMistral <- new_class(
  "ProviderMistral",
  parent = ProviderOpenAICompatible
)

chat_mistral_test <- function(
  system_prompt = NULL,
  model = "mistral-large-latest",
  params = NULL,
  ...,
  echo = "none"
) {
  params <- params %||% params()
  params <- modify_list(list(seed = 1014, temperature = 0), params)

  chat_mistral(
    system_prompt = system_prompt,
    model = model,
    params = params,
    ...,
    echo = echo
  )
}

method(base_request, ProviderMistral) <- function(provider) {
  req <- base_request(super(provider, ProviderOpenAICompatible))
  req <- ellmer_req_robustify(req, after = function(resp) {
    as.numeric(resp_header(resp, "ratelimitbysize-reset", NA))
  })
  req <- req_error(req, body = function(resp) {
    if (resp_content_type(resp) == "application/json") {
      resp_body_json(resp)$message
    }
  })
  req <- req_throttle(req, capacity = 1, fill_time_s = 1)
  req
}

method(chat_body, ProviderMistral) <- function(
  provider,
  stream = TRUE,
  turns = list(),
  tools = list(),
  type = NULL
) {
  body <- chat_body(
    super(provider, ProviderOpenAICompatible),
    stream = stream,
    turns = turns,
    tools = tools,
    type = type
  )

  # Mistral doensn't support stream options
  body$stream_options <- NULL

  body
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

# Models -----------------------------------------------------------------------

#' @export
#' @rdname chat_mistral
models_mistral <- function(api_key = mistral_key()) {
  provider <- ProviderMistral(
    name = "Mistral",
    model = "",
    base_url = mistral_base_url,
    credentials = function() api_key
  )

  req <- base_request(provider)
  req <- req_url_path_append(req, "/models")
  resp <- req_perform(req)

  json <- resp_body_json(resp)

  id <- map_chr(json$data, `[[`, "id")
  display_name <- map_chr(json$data, `[[`, "name")
  created_at <- as.POSIXct(map_int(json$data, `[[`, "created"))

  df <- data.frame(
    id = id,
    name = display_name,
    created_at = created_at
  )
  df <- cbind(df, match_prices("Mistral", df$id))
  df
}
