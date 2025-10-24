#' @include provider-openai.R
#' @include content.R
NULL

#' Chat with a model hosted by vLLM
#'
#' @description
#' [vLLM](https://docs.vllm.ai/en/latest/) is an open source library that
#' provides an efficient and convenient LLMs model server. You can use
#' `chat_vllm()` to connect to endpoints powered by vLLM.
#'
#' @inheritParams chat_openai
#' @param api_key `r lifecycle::badge("deprecated")` Use `credentials` instead.
#' @param credentials `r api_key_param("VLLM_API_KEY")`
#' @param model `r param_model(NULL, "vllm")`
#' @param params Common model parameters, usually created by [params()].
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
  api_headers = character()
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
    model = model,
    params = params,
    extra_args = api_args,
    credentials = credentials,
    extra_headers = api_headers
  )
  Chat$new(provider = provider, system_prompt = system_prompt, echo = echo)
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
  parent = ProviderOpenAI,
  package = "ellmer",
)

# Just like OpenAI but no strict
method(as_json, list(ProviderVllm, ToolDef)) <- function(provider, x, ...) {
  list(
    type = "function",
    "function" = compact(list(
      name = x@name,
      description = x@description,
      parameters = as_json(provider, x@arguments, ...)
    ))
  )
}

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

  req <- request(base_url)
  req <- req_auth_bearer_token(req, credentials())
  req <- req_url_path_append(req, "/v1/models")
  resp <- req_perform(req)
  json <- resp_body_json(resp)

  data.frame(
    id = map_chr(json$data, "[[", "id")
    # Not accurate?
    # created = .POSIXct(map_dbl(json$data, "[[", "created")),
    # owned_by = map_chr(json$data, "[[", "owned_by")
  )
}
