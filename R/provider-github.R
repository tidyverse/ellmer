#' Chat with a model hosted on the GitHub model marketplace
#'
#' @description
#' GitHub Models hosts a number of open source and OpenAI models. To access the
#' GitHub model marketplace, you will need to apply for and be accepted into the
#' beta access program. See <https://github.com/marketplace/models> for details.
#'
#' This function is a lightweight wrapper around [chat_openai()] with
#' the defaults tweaked for the GitHub Models marketplace.
#'
#' GitHub also suports the Azure AI Inference SDK, which you can use by setting
#' `base_url` to `"https://models.inference.ai.azure.com/"`. This endpoint was
#' used in \pkg{ellmer} v0.3.0 and earlier.
#'
#' @family chatbots
#' @param api_key `r lifecycle::badge("deprecated")` Use `credentials` instead.
#' @param credentials `r api_key_param("GITHUB_PAT")`
#' @param model `r param_model("gpt-4o")`
#' @param params Common model parameters, usually created by [params()].
#' @export
#' @inheritParams chat_openai
#' @inherit chat_openai return
#' @examples
#' \dontrun{
#' chat <- chat_github()
#' chat$chat("Tell me three jokes about statisticians")
#' }
chat_github <- function(
  system_prompt = NULL,
  base_url = "https://models.github.ai/inference/",
  api_key = NULL,
  credentials = NULL,
  model = NULL,
  params = NULL,
  api_args = list(),
  echo = NULL,
  api_headers = character()
) {
  check_installed("gitcreds")

  model <- set_default(model, "gpt-4.1")
  echo <- check_echo(echo)

  credentials <- as_credentials(
    "chat_github",
    function() github_key(),
    credentials = credentials,
    api_key = api_key
  )

  # https://docs.github.com/en/rest/models/inference?apiVersion=2022-11-28
  params <- params %||% params()

  chat_openai(
    system_prompt = system_prompt,
    base_url = base_url,
    credentials = credentials,
    model = model,
    params = params,
    api_args = api_args,
    echo = echo,
    api_headers = api_headers
  )
}

github_key <- function() {
  withCallingHandlers(
    gitcreds::gitcreds_get()$password,
    error = function(cnd) {
      cli::cli_abort(
        "Failed to find git credentials or GITHUB_PAT env var",
        parent = cnd
      )
    }
  )
}

#' @rdname chat_github
#' @export
models_github <- function(
  base_url = "https://models.github.ai/",
  api_key = NULL,
  credentials = NULL
) {
  credentials <- as_credentials(
    "models_github",
    function() github_key(),
    credentials = credentials,
    api_key = api_key
  )

  provider <- ProviderOpenAI(
    name = "github",
    model = "",
    credentials = credentials
  )

  req <- base_request(provider)
  req <- req_url_path_append(req, "/catalog/models")
  resp <- req_perform(req)

  json <- resp_body_json(resp)

  if (grepl("models.inference.ai.azure.com", base_url, fixed = TRUE)) {
    # Support listing models from the older Azure endpoint (ellmer <= 0.3.0)
    id <- map_chr(json, "[[", "name")
    publisher <- map_chr(json, "[[", "publisher")
    license <- map_chr(json, "[[", "license")
    task <- map_chr(json, "[[", "task")

    res <- data.frame(
      id = id,
      publisher = publisher,
      license = license,
      task = task
    )
    return(res)
  }

  id <- map_chr(json, "[[", "id")
  publisher <- map_chr(json, "[[", "publisher")
  registry <- map_chr(json, "[[", "registry")
  rate_limit_tier <- map_chr(json, "[[", "rate_limit_tier")
  version <- map_chr(json, "[[", "version")
  capabilities <- map_chr(
    map(json, "[[", "capabilities"),
    paste,
    collapse = ", "
  )

  data.frame(
    id = id,
    publisher = publisher,
    registry = registry,
    rate_limit_tier = rate_limit_tier,
    version = version,
    capabilities = capabilities
  )
}
