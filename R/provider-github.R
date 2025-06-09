#' Chat with a model hosted on the GitHub model marketplace
#'
#' @description
#' GitHub (via Azure) hosts a number of open source and OpenAI models.
#' To access the GitHub model marketplace, you will need to apply for and
#' be accepted into the beta access program. See
#' <https://github.com/marketplace/models> for details.
#'
#' This function is a lightweight wrapper around [chat_openai()] with
#' the defaults tweaked for the GitHub model marketplace.
#'
#' @family chatbots
#' @param api_key The API key to use for authentication. You generally should
#'   not supply this directly, but instead manage your GitHub credentials
#'   as described in <https://usethis.r-lib.org/articles/git-credentials.html>.
#'   For headless environments, this will also look in the `GITHUB_PAT`
#'   env var.
#' @param model `r param_model("gpt-4o")`
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
  base_url = "https://models.inference.ai.azure.com/",
  api_key = github_key(),
  model = NULL,
  seed = NULL,
  api_args = list(),
  echo = NULL
) {
  check_installed("gitcreds")

  model <- set_default(model, "gpt-4o")
  echo <- check_echo(echo)

  chat_openai(
    system_prompt = system_prompt,
    base_url = base_url,
    api_key = api_key,
    model = model,
    seed = seed,
    api_args = api_args,
    echo = echo
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
  base_url = "https://models.inference.ai.azure.com/",
  api_key = github_key()
) {
  provider <- ProviderOpenAI(
    name = "github",
    model = "",
    base_url = base_url,
    api_key = api_key
  )

  req <- base_request(provider)
  req <- req_url_path_append(req, "/models")
  resp <- req_perform(req)

  json <- resp_body_json(resp)

  id <- map_chr(json, "[[", "name")
  publisher <- map_chr(json, "[[", "publisher")
  license <- map_chr(json, "[[", "license")
  task <- map_chr(json, "[[", "task")

  data.frame(
    id = id,
    publisher = publisher,
    license = license,
    task = task
  )
}
