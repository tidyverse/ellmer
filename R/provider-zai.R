#' @include provider.R
#' @include provider-openai-compatible.R
#' @include content.R
#' @include turns.R
#' @include tools-def.R
NULL

#' Chat with a Z AI model
#'
#' @description
#' [Z AI](https://z.ai/) provides GLM (General Language Model) models
#' optimized for agentic coding and multi-step reasoning.
#'
#' ## API Endpoints
#'
#' Z AI offers two different API endpoints for different use cases:
#'
#' **Coding API (Default):**
#' - Endpoint: `https://api.z.ai/api/coding/paas/v4`
#' - Plan: GLM Coding Lite-Quarterly (subscription-based)
#' - Best for: Code generation, debugging, technical tasks
#' - Usage: `chat_zai()` (uses this endpoint by default)
#'
#' **Regular API:**
#' - Endpoint: `https://api.z.ai/api/paas/v4`
#' - Plan: Usage-based with credits
#' - Best for: General questions, conversations, non-coding tasks
#' - Usage: `chat_zai(base_url = "https://api.z.ai/api/paas/v4")`
#'
#' Both endpoints support the same models (glm-4.7, glm-4.6, glm-4.5, glm-4.5-Air)
#' and features (reasoning content, tool calling, structured data).
#'
#' ## Authentication
#'
#' You'll need an API key from https://z.ai/manage-apikey/apikey-list
#' Set it as the `ZAI_API_KEY` environment variable in your `.Renviron` file.
#'
#' @inheritParams chat_openai
#' @inherit chat_openai return
#' @param model `r param_model("glm-4.7", "zai")`
#' @param base_url The base URL to the endpoint. Defaults to the coding API
#'   endpoint. To use the regular API, set to `https://api.z.ai/api/paas/v4`.
#' @family chatbots
#' @export
#' @returns A [Chat] object.
#' @examples
#' if (nzchar(Sys.getenv("ZAI_API_KEY"))) {
#'   # Use coding API (default - for GLM Coding Lite-Quarterly plan)
#'   chat <- chat_zai()
#'   chat$chat("What is 2 + 2?")
#'
#'   # Use general API (for usage-based plans)
#'   chat <- chat_zai(base_url = "https://api.z.ai/api/paas/v4")
#'   chat$chat("What is 2 + 2?")
#' }
chat_zai <- function(
  system_prompt = NULL,
  base_url = "https://api.z.ai/api/coding/paas/v4",
  api_key = NULL,
  credentials = NULL,
  model = NULL,
  params = NULL,
  api_args = list(),
  api_headers = character(),
  echo = c("none", "output", "all")
) {
  model <- set_default(model, "glm-4.7")
  echo <- check_echo(echo)

  credentials <- as_credentials(
    "chat_zai",
    function() zai_key(),
    credentials = credentials,
    api_key = api_key
  )

  provider <- ProviderZAI(
    name = "Z AI",
    base_url = base_url,
    model = model,
    params = params %||% params(),
    extra_args = api_args,
    extra_headers = api_headers,
    credentials = credentials
  )
  Chat$new(provider = provider, system_prompt = system_prompt, echo = echo)
}

#' Create a Z AI chat object with deterministic defaults (for testing)
#'
#' @description
#' Convenience wrapper around [chat_zai()] intended for examples and tests.
#' Defaults to a terse system prompt and sets `temperature = 0` (unless you
#' explicitly supply a different temperature via `params`).
#'
#' @param ... Other arguments forwarded to [chat_zai()], such as `base_url`,
#'   `api_key`, `credentials`, `api_args`, and `api_headers`.
#' @param system_prompt System prompt for the assistant.
#' @param model Model name.
#' @param params Model parameters. If `params$temperature` is `NULL`, it is set
#'   to `0` for determinism.
#' @param echo Echo strategy. See [chat_zai()].
#' @export
#' @returns A [Chat] object.
#' @seealso [chat_zai()]
chat_zai_test <- function(
  ...,
  system_prompt = "Be terse.",
  model = "glm-4.7",
  params = NULL,
  echo = "none"
) {
  params <- params %||% params()
  params$temperature <- params$temperature %||% 0

  chat_zai(
    system_prompt = system_prompt,
    model = model,
    params = params,
    ...
  )
}

#' @rdname chat_zai
#' @export
models_zai <- function(
  base_url = "https://api.z.ai/api/coding/paas/v4",
  api_key = NULL,
  credentials = NULL
) {
  credentials <- as_credentials(
    "models_zai",
    function() zai_key(),
    credentials = credentials,
    api_key = api_key
  )

  provider <- ProviderZAI(
    name = "Z AI",
    model = "",
    base_url = base_url,
    credentials = credentials
  )

  req <- base_request(provider)
  req <- req_url_path_append(req, "/models")
  resp <- req_perform(req)

  json <- resp_body_json(resp)

  id <- map_chr(json$data, "[[", "id")
  created <- as.Date(.POSIXct(map_int(json$data, "[[", "created")))
  owned_by <- map_chr(json$data, "[[", "owned_by")

  df <- data.frame(
    id = id,
    created_at = created,
    owned_by = owned_by
  )
  df <- cbind(df, match_prices(provider@name, df$id))
  df[order(-xtfrm(df$created_at)), ]
}

# Provider class ---------------------------------------------------------------

ProviderZAI <- new_class(
  "ProviderZAI",
  parent = ProviderOpenAICompatible,
  properties = list(
    # No additional properties needed - inherits all from ProviderOpenAICompatible
  )
)

# Authentication helpers -------------------------------------------

zai_key <- function() {
  key_get("ZAI_API_KEY")
}

zai_key_exists <- function() {
  key_exists("ZAI_API_KEY")
}

# Override chat_params to handle Z AI specific parameters --------------

method(chat_params, ProviderZAI) <- function(provider, params) {
  params <- standardise_params(
    params,
    c(
      temperature = "temperature",
      top_p = "top_p",
      frequency_penalty = "frequency_penalty",
      presence_penalty = "presence_penalty",
      max_tokens = "max_tokens",
      max_completion_tokens = "max_completion_tokens",
      top_logprobs = "top_logprobs",
      n = "n"
    )
  )

  # Z AI doesn't require max_tokens to be set
  params$max_tokens <- NULL

  params
}

# Override value_tokens to extract reasoning tokens -----------------

method(value_tokens, ProviderZAI) <- function(provider, json) {
  usage <- json$usage

  # Extract reasoning tokens if available
  reasoning_tokens <- usage$completion_tokens_details$reasoning_tokens %||% 0
  cached_tokens <- usage$prompt_tokens_details$cached_tokens %||% 0

  tokens(
    input = (usage$prompt_tokens %||% 0) - cached_tokens,
    output = usage$completion_tokens %||% 0,
    cached_input = cached_tokens
  )
}

# Override value_turn to handle reasoning content -----------------

method(value_turn, ProviderZAI) <- function(
  provider,
  result,
  has_type = FALSE
) {
  contents <- lapply(result$choices[[1]]$message$content, function(content) {
    if (content$type == "text") {
      ContentText(content$text)
    } else if (content$type == "refusal") {
      ContentText(content$refusal)
    }
  })

  # Check for reasoning content
  if (!is.null(result$choices[[1]]$message$reasoning_content)) {
    contents <- c(
      list(ContentThinking(
        reasoning = result$choices[[1]]$message$reasoning_content
      )),
      contents
    )
  }

  tokens <- value_tokens(provider, result)
  cost <- get_token_cost(provider, tokens)

  AssistantTurn(
    contents = contents,
    json = result,
    tokens = unlist(tokens),
    cost = cost
  )
}
