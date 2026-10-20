#' @include provider-openai-compatible.R
NULL

#' Chat with one of the many models hosted on OpenRouter
#'
#' @description
#' `r support_badge("community")`
#'
#' Sign up at <https://openrouter.ai>.
#'
#' Support for features depends on the underlying model that you use; see
#' <https://openrouter.ai/models> for details. Use [models_openrouter()]
#' to list the available models along with their advertised token prices.
#'
#' @section Cost reporting:
#' OpenRouter's reported request cost is used when available, including zero.
#' If no valid charge is reported, ellmer estimates from advertised token
#' rates when both prompt and completion counts and the required rates are
#' available. Otherwise, the cost is unknown (`NA`), not zero.
#'
#' Estimates cover token rates only and may differ from the final routed
#' charge. Rates are cached for the session and may become stale. The first
#' fallback lookup is synchronous and may pause even an asynchronous chat. See
#' the [OpenRouter usage accounting documentation](https://openrouter.ai/docs/cookbook/administration/usage-accounting).
#'
#' An unknown turn cost makes cumulative chat and model totals unknown, while
#' known individual turn costs remain available. With a provider key configured
#' in OpenRouter (BYOK), the reported cost is OpenRouter's charge; the provider
#' may bill you separately.
#'
#' @export
#' @family chatbots
#' @param api_key `r lifecycle::badge("deprecated")` Use `credentials` instead.
#' @param credentials `r api_key_param("OPENROUTER_API_KEY")`
#' @param model `r param_model("gpt-5.6-terra")`
#' @param params Common model parameters, usually created by [params()].
#' @inheritParams chat_openai
#' @inheritParams chat_openai_compatible
#' @return `chat_openrouter()` returns a [Chat] object. `models_openrouter()`
#'   returns a data frame with one row per model, with its `id`, `created_at`,
#'   human-readable `name`, and `context_length`, plus the `cached_input`,
#'   `input`, and `output` prices OpenRouter advertises, in US dollars per
#'   million tokens. An `NA` price means that specific rate is unknown, e.g.
#'   because the model's cost is set by the underlying provider at request
#'   time; genuinely free models report `0`.

#' @examples
#' \dontrun{
#' chat <- chat_openrouter()
#' chat$chat("Tell me three jokes about statisticians")
#' }
chat_openrouter <- function(
  system_prompt = NULL,
  api_key = NULL,
  credentials = NULL,
  model = NULL,
  params = NULL,
  api_args = list(),
  echo = c("none", "output", "all"),
  api_headers = character(),
  strict = FALSE
) {
  model <- set_default(model, "gpt-5.6-terra")
  echo <- check_echo(echo)

  credentials <- as_credentials(
    "chat_openrouter",
    function() openrouter_key(),
    credentials = credentials,
    api_key = api_key
  )

  params <- params %||% params()

  provider <- ProviderOpenRouter(
    name = "OpenRouter",
    base_url = "https://openrouter.ai/api/v1",
    credentials = credentials,
    extra_headers = api_headers,
    preserve_thinking = TRUE,
    strict = strict
  )
  model <- Model(name = model, params = params, extra_args = api_args)
  Chat$new(
    provider = provider,
    model = model,
    system_prompt = system_prompt,
    echo = echo
  )
}

chat_openrouter_test <- function(..., echo = "none") {
  chat_openrouter(..., model = "openai/gpt-4o-mini-2024-07-18", echo = echo)
}

#' @rdname chat_openrouter
#' @export
#' @param base_url The base URL of the OpenRouter API. The `/models`
#'   endpoint used by `models_openrouter()` is public, so no credentials
#'   are required.
models_openrouter <- function(base_url = "https://openrouter.ai/api/v1") {
  check_string(base_url)

  # https://openrouter.ai/docs/models: the /models endpoint is public, so
  # use empty credentials rather than looking up OPENROUTER_API_KEY.
  provider <- ProviderOpenRouter(
    name = "OpenRouter",
    base_url = base_url,
    credentials = function() list()
  )

  models_list(provider)
}

ProviderOpenRouter <- new_class(
  "ProviderOpenRouter",
  parent = ProviderOpenAICompatible,
)

method(chat_params, ProviderOpenRouter) <- function(provider, params) {
  # https://openrouter.ai/docs/api-reference/parameters
  standardise_params(
    params,
    c(
      temperature = "temperature",
      top_p = "top_p",
      top_k = "top_k",
      frequency_penalty = "frequency_penalty",
      presence_penalty = "presence_penalty",
      seed = "seed",
      max_tokens = "max_tokens",
      logprobs = "log_probs",
      stop = "stop_sequences"
    )
  )
}

openrouter_key <- function() {
  key_get("OPENROUTER_API_KEY")
}

method(base_request, ProviderOpenRouter) <- function(provider) {
  req <- base_request(super(provider, ProviderOpenAICompatible))
  # https://openrouter.ai/docs/api-keys
  req <- req_headers(
    req,
    `HTTP-Referer` = "https://ellmer.tidyverse.org",
    `X-Title` = "ellmer"
  )

  req
}

method(value_turn, ProviderOpenRouter) <- function(
  provider,
  model,
  result,
  has_type = FALSE
) {
  # https://openrouter.ai/docs/errors
  check_openrouter_error(result$error)

  turn <- value_turn(
    super(provider, ProviderOpenAICompatible),
    model = model,
    result = result,
    has_type = has_type
  )

  # Reported charges take precedence over static token-rate estimates;
  # a missing or invalid charge is estimated from session-cached
  # advertised rates when usage reports usable token counts.
  turn@cost <- openrouter_actual_cost(result$usage$cost)
  if (is.na(turn@cost)) {
    turn@cost <- openrouter_estimate_cost(model@name, turn@tokens, result$usage)
  }

  turn
}

openrouter_actual_cost <- function(cost) {
  if (is.numeric(cost) && length(cost) == 1 && is.finite(cost)) {
    dollars(cost)
  } else {
    dollars(NA_real_)
  }
}

# Session-cached cost fallback ---------------------------------------------
#
# One lazy, session-wide table of the rates OpenRouter advertises backs all
# missing-charge estimates: the first eligible turn fetches it once, later
# turns for any model reuse it, and failed attempts are latched too so an
# outage can't trigger repeated lookups. Explicit models_openrouter() calls
# stay fresh and never read or write this cache.
openrouter_models_cache_factory <- function() {
  loaded <- FALSE
  models <- NULL
  function() {
    if (!loaded) {
      loaded <<- TRUE
      # The fallback runs after a completed reply, so an inherited long or
      # retrying transport could hang it: bound this lookup to at most five
      # seconds and one attempt (a shorter caller timeout stays shorter).
      # Scoped to this call, whatever its outcome.
      rlang::local_options(
        ellmer_timeout_s = min(getOption("ellmer_timeout_s", 5), 5),
        ellmer_max_tries = 1
      )
      models <<- tryCatch(models_openrouter(), error = function(cnd) NULL)
    }
    models
  }
}

openrouter_models_cache <- openrouter_models_cache_factory()

# A missing or invalid reported charge can be estimated from advertised
# rates, but only when usage reports both token counts: the parent token
# parsing substitutes zeros for absent counts, and a fabricated
# zero-dollar estimate is worse than an unknown cost.
openrouter_estimate_cost <- function(model, tokens, usage) {
  if (is.null(usage$prompt_tokens) || is.null(usage$completion_tokens)) {
    return(dollars(NA_real_))
  }

  models <- openrouter_models_cache()
  if (is.null(models)) {
    # The lookup failed earlier in this session; don't try again
    return(dollars(NA_real_))
  }

  i <- match(model, models$id)
  if (is.na(i)) {
    return(dollars(NA_real_))
  }

  # Advertised rates are USD per million tokens; a rate is required only
  # for token categories that were actually used.
  dollars(sum(
    openrouter_token_cost(tokens[["input"]], models$input[[i]]),
    openrouter_token_cost(tokens[["output"]], models$output[[i]]),
    openrouter_token_cost(tokens[["cached_input"]], models$cached_input[[i]])
  ))
}

openrouter_token_cost <- function(tokens, rate) {
  if (tokens == 0) {
    0
  } else if (is.na(rate)) {
    NA_real_
  } else {
    tokens * rate / 1e6
  }
}

method(stream_parse, ProviderOpenRouter) <- function(provider, event) {
  if (is.null(event) || identical(event$data, "[DONE]")) {
    return(NULL)
  }

  result <- jsonlite::parse_json(event$data)
  check_openrouter_error(result$error)
  result
}

check_openrouter_error <- function(error, call = caller_env()) {
  if (is.null(error)) {
    return()
  }
  message <- error$message
  if (is.null(error$metadata$raw$data)) {
    details <- NULL
  } else {
    details <- prettify(error$metadata$raw$data)
    # don't line wrap
    details <- gsub(" ", "\u00a0", details, fixed = TRUE)
  }

  abort(
    c(message, i = if (!is.null(details)) details),
    call = call
  )
}

method(chat_resp_stream, ProviderOpenRouter) <- function(provider, resp) {
  repeat {
    event <- resp_stream_sse(resp)
    if (is.null(event)) {
      break
    }

    # https://openrouter.ai/docs/responses#sse-streaming-comments
    if (!identical(event$data, character())) {
      break
    }
    Sys.sleep(0.1)
  }

  event
}

method(as_json, list(ProviderOpenRouter, ContentText)) <- function(
  provider,
  x,
  ...
) {
  if (identical(x@text, "")) {
    # Tool call requests can include a Content with empty text,
    # but it doesn't like it if you send this back
    NULL
  } else {
    list(type = "text", text = x@text)
  }
}

# Models -----------------------------------------------------------------

# Unlike the OpenAI-compatible listing, OpenRouter's /models response has
# no `owned_by` and reports per-token prices in its own `pricing` object,
# so parse it directly rather than matching against the static price
# snapshot.
method(models_list, ProviderOpenRouter) <- function(provider) {
  req <- base_request(provider)
  req <- req_url_path_append(req, "/models")
  resp <- req_perform(req)

  json <- resp_body_json(resp)
  models <- json$data %||% list()

  df <- data.frame(
    id = map_chr(models, \(x) x$id %||% NA_character_),
    created_at = as.Date(
      .POSIXct(
        map_dbl(models, \(x) {
          x$created %||% NA_real_
        })
      )
    ),
    name = map_chr(models, \(x) x$name %||% NA_character_),
    context_length = map_int(models, \(x) {
      as.integer(x$context_length %||% NA_integer_)
    })
  )
  df <- cbind(
    df,
    cached_input = map_dbl(models, \(x) {
      openrouter_rate(x$pricing$input_cache_read)
    }),
    input = map_dbl(models, \(x) openrouter_rate(x$pricing$prompt)),
    output = map_dbl(models, \(x) openrouter_rate(x$pricing$completion))
  )
  df[order(-xtfrm(df$created_at)), ]
}

# OpenRouter reports prices as strings in USD per token, and uses "-1" as
# a sentinel for models priced by the underlying provider at request time.
# Treat those, along with missing rates, as unknown; keep genuine zeros.
openrouter_rate <- function(rate) {
  if (
    length(rate) != 1L ||
      !(is.numeric(rate) || is.character(rate)) ||
      is.na(rate)
  ) {
    return(NA_real_)
  }

  out <- suppressWarnings(as.numeric(rate))
  if (is.na(out) || !is.finite(out) || out < 0) {
    return(NA_real_)
  }

  round(out * 1e6, digits = 6)
}
