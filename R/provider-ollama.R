#' Chat with a local Ollama model
#'
#' @description
#' To use `chat_ollama()` first download and install
#' [Ollama](https://ollama.com). Then install some models either from the
#' command line (e.g. with `ollama pull llama3.1`) or within R using
#' {[ollamar](https://hauselin.github.io/ollama-r/)} (e.g.
#' `ollamar::pull("llama3.1")`).
#'
#' This function is a lightweight wrapper around [chat_openai()] with
#' the defaults tweaked for ollama.
#'
#' ## Known limitations
#'
#' * Tool calling is not supported with streaming (i.e. when `echo` is
#'   `"text"` or `"all"`)
#' * Models can only use 2048 input tokens, and there's no way
#'   to get them to use more, except by creating a custom model with a
#'   different default.
#' * Tool calling generally seems quite weak, at least with the models I have
#'   tried it with.
#'
#' @inheritParams chat_openai
#' @param model `r param_model(NULL, "ollama")`
#' @param api_key Ollama doesn't require an API key for local usage and in most
#'   cases you do not need to provide an `api_key`.
#'
#'   However, if you're accessing an Ollama instance hosted behind a reverse
#'   proxy or secured endpoint that enforces bearer‐token authentication, you
#'   can set `api_key` (or the `OLLAMA_API_KEY` environment variable).
#' @inherit chat_openai return
#' @family chatbots
#' @export
#' @examples
#' \dontrun{
#' chat <- chat_ollama(model = "llama3.2")
#' chat$chat("Tell me three jokes about statisticians")
#' }
chat_ollama <- function(
  system_prompt = NULL,
  base_url = "http://localhost:11434",
  model,
  seed = NULL,
  api_args = list(),
  echo = NULL,
  api_key = NULL
) {
  if (!has_ollama(base_url)) {
    cli::cli_abort("Can't find locally running ollama.")
  }

  models <- models_ollama(base_url)$id

  if (missing(model)) {
    cli::cli_abort(c(
      "Must specify {.arg model}.",
      i = "Locally installed models: {.str {models}}."
    ))
  } else if (!model %in% models) {
    cli::cli_abort(
      c(
        "Model {.val {model}} is not installed locally.",
        i = "Run {.code ollama pull {model}} in your terminal or {.run ollamar::pull(\"{model}\")} in R to install the model.",
        i = "See locally installed models with {.run ellmer::models_ollama()}."
      )
    )
  }

  echo <- check_echo(echo)

  provider <- ProviderOllama(
    name = "Ollama",
    base_url = file.path(base_url, "v1"), ## the v1 portion of the path is added for openAI compatible API
    model = model,
    seed = seed,
    extra_args = api_args,
    # ollama doesn't require an API key for local usage, but one might be needed
    # if ollama is served behind a proxy (see #501)
    api_key = api_key %||% Sys.getenv("OLLAMA_API_KEY", "ollama")
  )

  Chat$new(provider = provider, system_prompt = system_prompt, echo = echo)
}

ProviderOllama <- new_class(
  "ProviderOllama",
  parent = ProviderOpenAI,
  properties = list(
    prop_redacted("api_key"),
    model = prop_string(),
    seed = prop_number_whole(allow_null = TRUE)
  )
)

chat_ollama_test <- function(..., model = "llama3.2:1b") {
  # model: Note that tests require a model with tool capabilities

  skip_if_no_ollama()
  testthat::skip_if_not(
    model %in% models_ollama()$id,
    sprintf("Ollama: model '%s' is not installed", model)
  )

  chat_ollama(..., model = model)
}

skip_if_no_ollama <- function() {
  if (!has_ollama()) {
    testthat::skip("ollama not found")
  }
}

#' @export
#' @rdname chat_ollama
models_ollama <- function(base_url = "http://localhost:11434") {
  req <- request(base_url)
  req <- req_url_path(req, "api/tags")
  resp <- req_perform(req)
  json <- resp_body_json(resp)

  names <- map_chr(json$models, "[[", "name")
  names <- gsub(":latest$", "", names)

  modified_at <- as.POSIXct(map_chr(json$models, "[[", "modified_at"))
  size <- map_dbl(json$models, "[[", "size")

  df <- data.frame(
    id = names,
    created_at = modified_at,
    size = size
  )
  df[order(-xtfrm(df$created_at)), ]
}

has_ollama <- function(base_url = "http://localhost:11434") {
  tryCatch(
    {
      req <- request(base_url)
      req <- req_url_path(req, "api/tags")
      req_perform(req)
      TRUE
    },
    httr2_error = function(cnd) FALSE
  )
}

method(as_json, list(ProviderOllama, TypeObject)) <- function(provider, x) {
  if (x@additional_properties) {
    cli::cli_abort("{.arg .additional_properties} not supported for Ollama.")
  }

  # Unlike OpenAI, Ollama uses the `required` field to list required tool args
  required <- map_lgl(x@properties, function(prop) prop@required)

  compact(list(
    type = "object",
    description = x@description %||% "",
    properties = as_json(provider, x@properties),
    required = as.list(names2(x@properties)[required]),
    additionalProperties = FALSE
  ))
}
