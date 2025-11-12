#' Chat with a model hosted on PortkeyAI
#'
#' @description
#' [PortkeyAI](https://portkey.ai/docs/product/ai-gateway/universal-api)
#' provides an interface (AI Gateway) to connect through its Universal API to a
#' variety of LLMs providers via a single endpoint.
#'
#' @family chatbots
#' @param model The model name, e.g. `@my-provider/my-model`.
#' @param api_key `r lifecycle::badge("deprecated")` Use `credentials` instead.
#' @param credentials `r api_key_param("PORTKEY_API_KEY")`
#' @param virtual_key `r lifecycle::badge("deprecated")`.
#'   Portkey now recommend supplying the model provider
#'   (formerly known as the `virtual_key`), in the model name, e.g.
#'   `@my-provider/my-model`. See
#'   <https://portkey.ai/docs/support/upgrade-to-model-catalog> for details.
#'
#'   For backward compatibility, the `PORTKEY_VIRTUAL_KEY` env var is still used
#'   if the model doesn't include a provider.
#' @export
#' @inheritParams chat_openai
#' @inherit chat_openai return
#' @examples
#' \dontrun{
#' chat <- chat_portkey()
#' chat$chat("Tell me three jokes about statisticians")
#' }
chat_portkey <- function(
  model,
  system_prompt = NULL,
  base_url = "https://api.portkey.ai/v1",
  api_key = NULL,
  credentials = NULL,
  virtual_key = deprecated(),
  params = NULL,
  api_args = list(),
  echo = NULL,
  api_headers = character()
) {
  check_string(model)
  echo <- check_echo(echo)

  if (lifecycle::is_present(virtual_key)) {
    lifecycle::deprecate_warn(
      when = "0.4.0",
      what = "chat_portkey(virtual_key=)",
      with = "chat_portkey(model=)",
    )
    check_string(virtual_key, allow_null = TRUE)
  } else {
    virtual_key <- NULL
  }

  # For backward compatibility
  if (!grepl("^@", model)) {
    virtual_key <- virtual_key %||% key_get("PORTKEY_VIRTUAL_KEY")
    model <- paste0("@", virtual_key, "/", model)
  }

  credentials <- as_credentials(
    "chat_portkey",
    function() portkey_key(),
    credentials = credentials,
    api_key = api_key
  )

  params <- params %||% params()
  provider <- ProviderPortkeyAI(
    name = "PortkeyAI",
    base_url = base_url,
    model = model,
    params = params,
    extra_args = api_args,
    credentials = credentials,
    extra_headers = api_headers
  )
  Chat$new(provider = provider, system_prompt = system_prompt, echo = echo)
}

chat_portkey_test <- function(
  ...,
  model = "@open-ai-virtual-7f0dcd/gpt-4.1-nano",
  params = NULL,
  echo = "none"
) {
  params <- params %||% params()
  params$temperature <- params$temperature %||% 0

  chat_portkey(model = model, params = params, ..., echo = echo)
}

ProviderPortkeyAI <- new_class(
  "ProviderPortkeyAI",
  parent = ProviderOpenAICompatible
)

portkey_key <- function() {
  key_get("PORTKEY_API_KEY")
}

method(base_request, ProviderPortkeyAI) <- function(provider) {
  req <- request(provider@base_url)

  req <- ellmer_req_credentials(
    req,
    provider@credentials(),
    "x-portkey-api-key"
  )
  req <- ellmer_req_robustify(req)
  req <- ellmer_req_user_agent(req)
  req <- base_request_error(provider, req)
  req
}


#' @export
#' @rdname chat_portkey
models_portkey <- function(
  base_url = "https://api.portkey.ai/v1",
  api_key = portkey_key()
) {
  provider <- ProviderPortkeyAI(
    name = "PortkeyAI",
    model = "",
    base_url = base_url,
    credentials = function() api_key
  )

  req <- base_request(provider)
  req <- req_url_path_append(req, "/models")
  resp <- req_perform(req)

  json <- resp_body_json(resp)

  id <- map_chr(json$data, "[[", "id")
  slug <- map_chr(json$data, "[[", "slug")

  df <- data.frame(
    id = id,
    slug = slug
  )
  df
}
