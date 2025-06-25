#' Chat with a model hosted on PortkeyAI
#'
#' @description
#' [PortkeyAI](https://portkey.ai/docs/product/ai-gateway/universal-api)
#' provides an interface (AI Gateway) to connect through its Universal API to a
#' variety of LLMs providers with a single endpoint.
#'
#' ## Authentication
#' API keys together with configurations of LLM providers are
#' stored inside Portkey application.
#'
#' @family chatbots
#' @param api_key `r api_key_param("PORTKEY_API_KEY")`
#' @param model `r param_model("gpt-4o", "openai")`
#' @param virtual_key A virtual identifier storing LLM provider's API key. See
#'   [documentation](https://portkey.ai/docs/product/ai-gateway/virtual-keys).
#'   Can be read from the `PORTKEY_VIRTUAL_KEY` environment variable.
#' @export
#' @inheritParams chat_openai
#' @inherit chat_openai return
#' @examples
#' \dontrun{
#' chat <- chat_portkey(virtual_key = Sys.getenv("PORTKEY_VIRTUAL_KEY"))
#' chat$chat("Tell me three jokes about statisticians")
#' }
chat_portkey <- function(
  system_prompt = NULL,
  base_url = "https://api.portkey.ai/v1",
  api_key = portkey_key(),
  virtual_key = portkey_virtual_key(),
  model = NULL,
  params = NULL,
  api_args = list(),
  echo = NULL
) {
  model <- set_default(model, "gpt-4o")
  echo <- check_echo(echo)

  params <- params %||% params()
  provider <- ProviderPortkeyAI(
    name = "PortkeyAI",
    base_url = base_url,
    model = model,
    params = params,
    extra_args = api_args,
    api_key = api_key,
    virtual_key = virtual_key
  )
  Chat$new(provider = provider, system_prompt = system_prompt, echo = echo)
}

chat_portkey_test <- function(
  ...,
  model = "gpt-4o-mini",
  params = NULL,
  echo = "none"
) {
  params <- params %||% params()
  params$temperature <- params$temperature %||% 0

  chat_portkey(model = model, params = params, ..., echo = echo)
}

ProviderPortkeyAI <- new_class(
  "ProviderPortkeyAI",
  parent = ProviderOpenAI,
  properties = list(
    virtual_key = prop_string(allow_null = TRUE)
  )
)

portkey_key_exists <- function() {
  key_exists("PORTKEY_API_KEY")
}

portkey_key <- function() {
  key_get("PORTKEY_API_KEY")
}

portkey_virtual_key <- function() {
  val <- Sys.getenv("PORTKEY_VIRTUAL_KEY")
  if (!identical(val, "")) {
    val
  } else {
    NULL
  }
}

method(base_request, ProviderPortkeyAI) <- function(provider) {
  req <- request(provider@base_url)
  req <- httr2::req_headers(
    req,
    `x-portkey-api-key` = provider@api_key,
    `x-portkey-virtual-key` = provider@virtual_key
  )
  req <- req_retry(req, max_tries = 2)
  req <- ellmer_req_timeout(req, stream)
  req <- ellmer_req_user_agent(req)
  req <- base_request_error(provider, req)
  req
}


#' @export
#' @rdname chat_portkey
models_portkey <- function(
  base_url = "https://api.portkey.ai/v1",
  api_key = portkey_key(),
  virtual_key = NULL
) {
  provider <- ProviderPortkeyAI(
    name = "PortkeyAI",
    model = "",
    base_url = base_url,
    api_key = api_key,
    virtual_key = virtual_key
  )

  req <- base_request(provider)
  req <- req_url_path_append(req, "/models")
  resp <- req_perform(req)

  json <- resp_body_json(resp)

  id <- map_chr(json$data, "[[", "id")
  created_at <- as.POSIXct(map_dbl(json$data, "[[", "created_at"))

  df <- data.frame(
    id = id,
    created_at = created_at
  )
  df[order(-xtfrm(df$created_at)), ]
}
