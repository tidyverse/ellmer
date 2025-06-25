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
#' @param api_key `r api_key_param("VLLM_API_KEY")`
#' @param model `r param_model(NULL, "vllm")`
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
  seed = NULL,
  api_args = list(),
  api_key = vllm_key(),
  echo = NULL
) {
  check_string(base_url)
  check_string(api_key)
  if (missing(model)) {
    models <- models_vllm(base_url, api_key)$id
    cli::cli_abort(c(
      "Must specify {.arg model}.",
      i = "Available models: {.str {models}}."
    ))
  }
  echo <- check_echo(echo)

  provider <- ProviderVllm(
    name = "VLLM",
    base_url = base_url,
    model = model,
    seed = seed,
    extra_args = api_args,
    api_key = api_key
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
method(as_json, list(ProviderVllm, ToolDef)) <- function(provider, x) {
  list(
    type = "function",
    "function" = compact(list(
      name = x@name,
      description = x@description,
      parameters = as_json(provider, x@arguments)
    ))
  )
}

vllm_key <- function() {
  key_get("VLLM_API_KEY")
}

#' @export
#' @rdname chat_vllm
models_vllm <- function(base_url, api_key = vllm_key()) {
  req <- request(base_url)
  req <- req_auth_bearer_token(req, api_key)
  req <- req_url_path(req, "/v1/models")
  resp <- req_perform(req)
  json <- resp_body_json(resp)

  data.frame(
    id = map_chr(json$data, "[[", "id")
    # Not accurate?
    # created = .POSIXct(map_dbl(json$data, "[[", "created")),
    # owned_by = map_chr(json$data, "[[", "owned_by")
  )
}
