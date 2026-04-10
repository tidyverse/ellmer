#' @include provider-openai-compatible.R
#' @include content.R
NULL

#' Chat with a local LM Studio model
#'
#' @description
#' To use `chat_lmstudio()` first download and install
#' [LM Studio](https://lmstudio.ai). Then load a model using the LM Studio
#' GUI and start the local server. To learn more about running LM Studio
#' locally, see <https://lmstudio.ai/docs/developer/core/server>/.
#'
#' Built on top of [chat_openai_compatible()].
#'
#' @examples
#' \dontrun{
#' # https://lmstudio.ai/models/zai-org/glm-4.7-flash
#' chat <- chat_lmstudio(model = "zai-org/glm-4.7-flash")
#' chat$chat("Tell me three jokes about statisticians")
#' }
#'
#' @inheritParams chat_openai
#' @param model `r param_model(NULL, "lmstudio")`
#' @param credentials LM Studio doesn't require credentials for local usage
#'   and in most cases you do not need to provide `credentials`.
#'
#'   However, if you're accessing an LM Studio instance hosted behind a
#'   reverse proxy or secured endpoint that enforces bearer-token
#'   authentication, you can set the `LMSTUDIO_API_KEY` environment variable
#'   or provide a callback function to `credentials`.
#' @param params Common model parameters, usually created by [params()].
#'
#' @inherit chat_openai return
#'
#' @family chatbots
#' @export
chat_lmstudio <- function(
  system_prompt = NULL,
  base_url = Sys.getenv("LMSTUDIO_BASE_URL", "http://localhost:1234"),
  model,
  params = NULL,
  api_args = list(),
  echo = NULL,
  credentials = NULL,
  api_headers = character()
) {
  credentials <- lmstudio_credentials(credentials)

  if (!has_lmstudio(base_url, credentials)) {
    cli::cli_abort("Can't find locally running LM Studio.")
  }

  models <- models_lmstudio(base_url, credentials)$id

  if (missing(model)) {
    cli::cli_abort(c(
      "Must specify {.arg model}.",
      i = "Locally available models: {.str {models}}."
    ))
  } else if (!model %in% models) {
    cli::cli_abort(
      c(
        "Model {.val {model}} is not available in LM Studio.",
        i = "Download the model using the LM Studio GUI.",
        i = "See locally available models with {.run ellmer::models_lmstudio()}."
      )
    )
  }

  echo <- check_echo(echo)

  provider <- ProviderLMStudio(
    name = "LM Studio",
    base_url = file.path(base_url, "v1"),
    model = model,
    params = params %||% params(),
    extra_args = api_args,
    credentials = credentials,
    extra_headers = api_headers
  )

  Chat$new(provider = provider, system_prompt = system_prompt, echo = echo)
}

ProviderLMStudio <- new_class(
  "ProviderLMStudio",
  parent = ProviderOpenAICompatible,
  properties = list(
    model = prop_string()
  )
)

lmstudio_credentials <- function(credentials = NULL) {
  as_credentials(
    "chat_lmstudio",
    function() Sys.getenv("LMSTUDIO_API_KEY", ""),
    credentials = credentials
  )
}

method(chat_params, ProviderLMStudio) <- function(provider, params) {
  # https://lmstudio.ai/docs/developer/openai-compat/chat-completions#supported-payload-parameters
  standardise_params(
    params,
    c(
      frequency_penalty = "frequency_penalty",
      max_tokens = "max_tokens",
      presence_penalty = "presence_penalty",
      seed = "seed",
      stop = "stop_sequences",
      temperature = "temperature",
      top_k = "top_k",
      top_p = "top_p"
    )
  )
}

chat_lmstudio_test <- function(..., model = NULL, echo = "none") {
  skip_if_no_lmstudio()
  if (is.null(model)) {
    models <- models_lmstudio()$id
    if (length(models) == 0) {
      testthat::skip("No models loaded in LM Studio")
    }
    models <- union(
      # Prefer gemma4 or glm-4.7-flash if they're available
      intersect(c("google/gemma-4-26b-a4b", "zai-org/glm-4.7-flash"), models),
      models
    )
    model <- models[[1]]
  }
  chat_lmstudio(..., model = model, echo = echo)
}

skip_if_no_lmstudio <- function() {
  if (!has_lmstudio()) {
    testthat::skip("LM Studio not found")
  }
}

#' @export
#' @rdname chat_lmstudio
models_lmstudio <- function(
  base_url = "http://localhost:1234",
  credentials = NULL
) {
  credentials <- as_credentials(
    "models_lmstudio",
    function() Sys.getenv("LMSTUDIO_API_KEY", ""),
    credentials = credentials
  )

  req <- request(base_url)
  req <- ellmer_req_credentials(req, credentials(), "Authorization")
  req <- req_url_path_append(req, "/v1/models")
  resp <- req_perform(req)
  json <- resp_body_json(resp)

  data.frame(
    id = map_chr(json$data, "[[", "id")
  )
}

has_lmstudio <- function(
  base_url = "http://localhost:1234",
  credentials = lmstudio_credentials()
) {
  check_credentials(credentials)

  tryCatch(
    {
      req <- request(base_url)
      req <- ellmer_req_credentials(req, credentials(), "Authorization")
      req <- req_url_path_append(req, "/v1/models")
      req_perform(req)
      TRUE
    },
    httr2_error = function(cnd) FALSE
  )
}
