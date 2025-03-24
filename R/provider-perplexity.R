#' Chat with a model hosted on perplexity.ai
#'
#' @description
#' Sign up at <https://www.perplexity.ai>.
#'
#' Perplexity AI is a platform for running LLMs that are capable of
#' searching the web in real-time to help them answer questions with
#' information that may not have been available when the model was
#' trained.
#'
#' This function is a lightweight wrapper around [chat_openai()] with
#' the defaults tweaked for Perplexity AI.
#'
#' @export
#' @family chatbots
#' @param api_key The API key to use for authentication. You generally should
#'   not supply this directly, but instead set the `PERPLEXITY_API_KEY` environment
#'   variable.
#' @inheritParams chat_openai
#' @inherit chat_openai return
#' @examples
#' \dontrun{
#' chat <- chat_perplexity()
#' chat$chat("Tell me three jokes about statisticians")
#' }
chat_perplexity <- function(system_prompt = NULL,
                            turns = NULL,
                            base_url = "https://api.perplexity.ai/",
                            api_key = perplexity_key(),
                            model = NULL,
                            seed = NULL,
                            api_args = list(),
                            echo = NULL) {

  turns <- normalize_turns(turns, system_prompt)
  model <- set_default(model, "llama-3.1-sonar-small-128k-online")
  echo <- check_echo(echo)

  if (is_testing() && is.null(seed)) {
    seed <- seed %||% 1014
  }

  provider <- ProviderPerplexity(
    base_url = base_url,
    model = model,
    seed = seed,
    extra_args = api_args,
    api_key = api_key
  )

  Chat$new(provider = provider, turns = turns, echo = echo)
}

perplexity_key <- function() {
  key_get("PERPLEXITY_API_KEY")
}

ProviderPerplexity <- new_class(
  "ProviderPerplexity",
  parent = ProviderOpenAI
)

method(stream_parse, ProviderPerplexity) <- function(provider, event) {
  if (is.null(event) || identical(event$data, "[DONE]")) {
    return(NULL)
  }

  jsonlite::parse_json(event$data)
}
