#' @include provider-openai.R
NULL

chat_open_router <- function(
  system_prompt = NULL,
  turns = NULL,
  api_key = openrouter_key(),
  model = NULL,
  seed = NULL,
  api_args = list(),
  echo = c("none", "text", "all")
) {
  turns <- normalize_turns(turns, system_prompt)
  model <- set_default(model, "gpt-4o")
  echo <- check_echo(echo)

  if (is_testing() && is.null(seed)) {
    seed <- seed %||% 1014
  }

  provider <- ProviderOpenRouter(
    base_url = "https://openrouter.ai/api/v1",
    model = model,
    seed = seed,
    extra_args = api_args,
    api_key = api_key
  )
  Chat$new(provider = provider, turns = turns, echo = echo)
}

ProviderOpenRouter <- new_class(
  "ProviderOpenRouter",
  parent = ProviderOpenAI,
)

openrouter_key <- function() {
  key_get("OPENROUTER_API_KEY")
}

method(chat_request, ProviderOpenRouter) <- function(
  provider,
  stream = TRUE,
  turns = list(),
  tools = list(),
  type = NULL,
  extra_args = list()
) {
  req <- chat_request(
    super(provider, ProviderOpenAI),
    stream = stream,
    turns = turns,
    tools = tools,
    type = type,
    extra_args = extra_args
  )

  # https://openrouter.ai/docs/api-keys
  req <- req_headers(
    req,
    `HTTP-Referer` = "https://ellmer.tidyverse.org",
    `X-Title` = "ellmer"
  )

  req
}

method(stream_parse, ProviderOpenRouter) <- function(provider, event) {
  if (is.null(event)) {
    cli::cli_abort("Connection closed unexpectedly")
  }

  if (identical(event$data, "[DONE]")) {
    return(NULL)
  }

  data <- jsonlite::parse_json(event$data)

  if (!is.null(data$error)) {
    message <- data$error$message
    details <- gsub(" ", "\u00a0", prettify(data$error$metadata$raw$data %||% ""), fixed = TRUE)
    cli::cli_abort(c("{message}", i = "{details}"))
  }

  data
}

method(chat_resp_stream, ProviderOpenRouter) <- function(provider, resp) {
  repeat({
    event <- resp_stream_sse(resp)
    if (is.null(event)) {
      break
    }

    if (!identical(event$data, character())) {
      break
    }
    Sys.sleep(0.1)
  })

  event
}

method(as_json, list(ProviderOpenRouter, ContentText)) <- function(provider, x) {
  if (identical(x@text, "")) {
    # Tool call requests can include a Content with empty text,
    # but it doesn't like it if you send this back
    NULL
  } else {
    list(type = "text", text = x@text)
  }
}
