#' @include provider-openai-compatible.R
#' @include content.R
NULL

#' Chat with a model hosted by vLLM
#'
#' @description
#' `r support_badge("community")`
#'
#' [vLLM](https://docs.vllm.ai/en/latest/) is an open source library that
#' provides an efficient and convenient LLMs model server. You can use
#' `chat_vllm()` to connect to endpoints powered by vLLM.
#'
#' Uses OpenAI compatible API via `chat_openai_compatible()`.
#'
#' @inheritParams chat_openai
#' @param api_key `r lifecycle::badge("deprecated")` Use `credentials` instead.
#' @param credentials `r api_key_param("VLLM_API_KEY")`
#' @param model `r param_model(NULL, "vllm")`
#' @param params Common model parameters, usually created by [params()].
#' @param strict If `TRUE`, use OpenAI's strict-mode tool and structured-output
#'   schemas (all properties `required`, optional ones nullable). Whether these
#'   work depends on the model you're serving; vLLM itself ignores the `strict`
#'   request field.
#' @inherit chat_openai return
#' @export
#' @examples
#' \dontrun{
#' chat <- chat_vllm("http://my-vllm.com")
#' chat$chat("Tell me three jokes about statisticians")
#' }
chat_vllm <- function(
  base_url,
  system_prompt = NULL,
  model,
  params = NULL,
  api_args = list(),
  api_key = NULL,
  credentials = NULL,
  echo = NULL,
  api_headers = character(),
  strict = FALSE
) {
  check_string(base_url)

  credentials <- as_credentials(
    "chat_vllm",
    function() vllm_key(),
    credentials = credentials,
    api_key = api_key
  )

  check_string(credentials())
  if (missing(model)) {
    models <- models_vllm(base_url, credentials = credentials)$id
    cli::cli_abort(c(
      "Must specify {.arg model}.",
      i = "Available models: {.str {models}}."
    ))
  }
  echo <- check_echo(echo)

  # https://docs.vllm.ai/en/latest/serving/openai_compatible_server.html
  params <- params %||% params()

  provider <- ProviderVllm(
    name = "VLLM",
    base_url = base_url,
    credentials = credentials,
    extra_headers = api_headers,
    strict = strict
  )
  model <- Model(name = model, params = params, extra_args = api_args)
  Chat$new(
    provider = provider,
    model = model,
    system_prompt = system_prompt,
    echo = echo
  )
}

chat_vllm_test <- function(..., echo = "none") {
  chat_vllm(
    base_url = "https://llm.nrp-nautilus.io/",
    ...,
    model = "llama3",
    echo = echo
  )
}

ProviderVllm <- new_class(
  "ProviderVllm",
  parent = ProviderOpenAICompatible,
  package = "ellmer",
)

vllm_key <- function() {
  key_get("VLLM_API_KEY")
}

#' @export
#' @rdname chat_vllm
models_vllm <- function(base_url, api_key = NULL, credentials = NULL) {
  credentials <- as_credentials(
    "models_vllm",
    function() vllm_key(),
    credentials = credentials,
    api_key = api_key
  )

  provider <- ProviderVllm(
    name = "VLLM",
    base_url = base_url,
    credentials = credentials
  )

  models_list(provider)
}

method(models_list, ProviderVllm) <- function(provider) {
  req <- base_request(provider)
  req <- req_url_path_append(req, "/v1/models")
  resp <- req_perform(req)
  json <- resp_body_json(resp)

  data.frame(
    id = map_chr(json$data, "[[", "id")
  )
}
