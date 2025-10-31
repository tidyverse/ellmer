#' Standard model parameters
#'
#' @description
#' This helper function makes it easier to create a list of parameters used
#' across many models. The parameter names are automatically standardised and
#' included in the correctly place in the API call.
#'
#' Note that parameters that are not supported by a given provider will generate
#' a warning, not an error. This allows you to use the same set of parameters
#' across multiple providers.
#'
#' @param temperature Temperature of the sampling distribution.
#' @param top_p The cumulative probability for token selection.
#' @param top_k The number of highest probability vocabulary tokens to keep.
#' @param frequency_penalty Frequency penalty for generated tokens.
#' @param presence_penalty Presence penalty for generated tokens.
#' @param seed Seed for random number generator.
#' @param max_tokens Maximum number of tokens to generate.
#' @param log_probs Include the log probabilities in the output?
#' @param stop_sequences A character vector of tokens to stop generation on.
#' @param reasoning_effort,reasoning_tokens How much effort to spend thinking?
#'   `ressoning_effort` is a string, like "low", "medium", "high".
#'   `reasoning_tokens` is an integer, giving a maximum token budget.
#'   Each provider only takes one of these two parameters.
#' @param ... Additional named parameters to send to the provider.
#' @export
params <- function(
  temperature = NULL,
  top_p = NULL,
  top_k = NULL,
  frequency_penalty = NULL,
  presence_penalty = NULL,
  seed = NULL,
  max_tokens = NULL,
  log_probs = NULL,
  stop_sequences = NULL,
  reasoning_effort = NULL,
  reasoning_tokens = NULL,
  ...
) {
  check_number_decimal(temperature, allow_null = TRUE, min = 0)
  check_number_decimal(top_p, allow_null = TRUE, min = 0)
  check_number_whole(top_k, allow_null = TRUE, min = 0)
  check_number_decimal(frequency_penalty, allow_null = TRUE)
  check_number_decimal(presence_penalty, allow_null = TRUE)
  check_number_whole(seed, allow_null = TRUE)
  check_number_whole(max_tokens, allow_null = TRUE, min = 1)
  check_bool(log_probs, allow_null = TRUE)
  check_character(stop_sequences, allow_null = TRUE)
  check_string(reasoning_effort, allow_null = TRUE)
  check_number_whole(reasoning_tokens, min = 0, allow_null = TRUE)

  compact(list2(
    temperature = temperature,
    top_p = top_p,
    top_k = top_k,
    frequency_penalty = frequency_penalty,
    presence_penalty = presence_penalty,
    seed = seed,
    max_tokens = max_tokens,
    log_probs = log_probs,
    stop_sequences = stop_sequences,
    reasoning_effort = reasoning_effort,
    reasoning_tokens = reasoning_tokens,
    extra_args = list2(...)
  ))
}

standardise_params <- function(params, provider_params) {
  standard <- params[names(params) != "extra_args"]

  unknown <- setdiff(names(standard), provider_params)
  if (length(unknown) > 0) {
    cli::cli_warn("Ignoring unsupported parameters: {.str {unknown}}")
    standard <- standard[names(standard) %in% provider_params]
  }

  names(standard) <- names(provider_params)[match(
    names(standard),
    provider_params
  )]

  c(standard, params$extra_args)
}

## To implement a `chat_params()` method for a new provider, use this template:
# method(chat_params, ProviderNew) <- function(provider, params) {
#   # <link to api docs>
#   standardise_params(
#     params,
#     c(
#       paramFromAPI = "ellmer_params_name",
#       maxTokens = "max_tokens",
#       # ... all supported parameters that overlap with `params()` ...
#     )
#   )
# }
