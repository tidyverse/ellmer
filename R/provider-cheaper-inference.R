#' @include provider-openai-compatible.R
NULL

#' Chat with a model hosted on Cheaper Inference
#'
#' @description
#' `r support_badge("community")`
#'
#' Sign up at <https://cheaperinference.com>.
#'
#' Cheaper Inference is an OpenAI-compatible gateway. One API key gives access
#' to models from several labs. This function uses the OpenAI-compatible API via
#' `chat_openai_compatible()` with the defaults set for Cheaper Inference.
#'
#' @export
#' @family chatbots
#' @param api_key `r lifecycle::badge("deprecated")` Use `credentials` instead.
#' @param credentials `r api_key_param("CHEAPER_INFERENCE_API_KEY")`
#' @param model `r param_model("gpt-5.4-mini")`
#' @param params Common model parameters, usually created by [params()].
#' @inheritParams chat_openai
#' @inherit chat_openai return
#' @examples
#' \dontrun{
#' chat <- chat_cheaper_inference()
#' chat$chat("Tell me three jokes about statisticians")
#' }
chat_cheaper_inference <- function(
  system_prompt = NULL,
  base_url = "https://api.cheaperinference.com/v1",
  api_key = NULL,
  credentials = NULL,
  model = NULL,
  params = NULL,
  api_args = list(),
  echo = NULL,
  api_headers = character()
) {
  model <- set_default(model, "gpt-5.4-mini")
  echo <- check_echo(echo)

  credentials <- as_credentials(
    "chat_cheaper_inference",
    function() cheaper_inference_key(),
    credentials = credentials,
    api_key = api_key
  )

  params <- params %||% params()

  provider <- ProviderCheaperInference(
    name = "Cheaper Inference",
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

ProviderCheaperInference <- new_class(
  "ProviderCheaperInference",
  parent = ProviderOpenAICompatible,
)

cheaper_inference_key <- function() {
  key_get("CHEAPER_INFERENCE_API_KEY")
}
