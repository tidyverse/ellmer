#' @include provider-openai-compatible.R
NULL

#' Chat with a model hosted on Groq
#'
#' @description
#' Sign up at <https://groq.com>.
#'
#' Built on top of [chat_openai_compatible()].
#'
#' @export
#' @family chatbots
#' @param api_key `r lifecycle::badge("deprecated")` Use `credentials` instead.
#' @param credentials `r api_key_param("GROQ_API_KEY")`
#' @param model `r param_model("llama-3.1-8b-instant")`
#' @param params Common model parameters, usually created by [params()].
#' @inheritParams chat_openai
#' @inherit chat_openai return
#' @examples
#' \dontrun{
#' chat <- chat_groq()
#' chat$chat("Tell me three jokes about statisticians")
#' }
chat_groq <- function(
  system_prompt = NULL,
  base_url = "https://api.groq.com/openai/v1",
  api_key = NULL,
  credentials = NULL,
  model = NULL,
  params = NULL,
  api_args = list(),
  echo = NULL,
  api_headers = character()
) {
  model <- set_default(model, "llama-3.1-8b-instant")
  echo <- check_echo(echo)

  credentials <- as_credentials(
    "chat_groq",
    function() groq_key(),
    credentials = credentials,
    api_key = api_key
  )

  # https://console.groq.com/docs/api-reference#chat-create (same as OpenAI)
  params <- params %||% params()

  provider <- ProviderGroq(
    name = "Groq",
    base_url = base_url,
    model = model,
    params = params,
    extra_args = api_args,
    credentials = credentials,
    extra_headers = api_headers
  )
  Chat$new(provider = provider, system_prompt = system_prompt, echo = echo)
}

ProviderGroq <- new_class("ProviderGroq", parent = ProviderOpenAICompatible)


method(as_json, list(ProviderGroq, ToolDef)) <- function(provider, x, ...) {
  list(
    type = "function",
    "function" = compact(list(
      name = x@name,
      description = x@description,
      parameters = as_json(provider, x@arguments, ...)
    ))
  )
}

groq_key <- function() {
  key_get("GROQ_API_KEY")
}

# Batched requests -------------------------------------------------------------

# https://console.groq.com/docs/batch
method(has_batch_support, ProviderGroq) <- function(provider) {
  TRUE
}

method(batch_submit, ProviderGroq) <- function(
  provider,
  conversations,
  type = NULL
) {
  path <- withr::local_tempfile(fileext = ".jsonl")

  requests <- map(seq_along(conversations), function(i) {
    body <- chat_body(
      provider,
      stream = FALSE,
      turns = conversations[[i]],
      type = type
    )
    list(
      custom_id = paste0("chat-", i),
      method = "POST",
      url = "/v1/chat/completions",
      body = body
    )
  })
  json <- map_chr(requests, to_json)
  writeLines(json, path)

  uploaded <- groq_upload_file(provider, path)

  req <- base_request(provider)
  req <- req_url_path_append(req, "batches")
  req <- req_body_json(
    req,
    list(
      input_file_id = uploaded$id,
      endpoint = "/v1/chat/completions",
      completion_window = "24h"
    )
  )

  resp <- req_perform(req)
  resp_body_json(resp)
}

method(batch_poll, ProviderGroq) <- function(provider, batch) {
  req <- base_request(provider)
  req <- req_url_path_append(req, "batches", batch$id)

  resp <- req_perform(req)
  resp_body_json(resp)
}

method(batch_status, ProviderGroq) <- function(provider, batch) {
  terminal_states <- c("completed", "failed", "expired", "cancelled")

  total <- batch$request_counts$total %||% 0L
  completed <- batch$request_counts$completed %||% 0L
  failed <- batch$request_counts$failed %||% 0L

  list(
    working = !(batch$status %in% terminal_states),
    n_processing = max(total - completed - failed, 0L),
    n_succeeded = completed,
    n_failed = failed
  )
}

method(batch_retrieve, ProviderGroq) <- function(provider, batch) {
  json <- list()

  if (length(batch$output_file_id) == 1 && nzchar(batch$output_file_id)) {
    path_output <- withr::local_tempfile()
    groq_download_file(provider, batch$output_file_id, path_output)
    json <- read_ndjson(path_output, fallback = groq_json_fallback)
  }

  if (length(batch$error_file_id) == 1 && nzchar(batch$error_file_id)) {
    path_error <- withr::local_tempfile()
    groq_download_file(provider, batch$error_file_id, path_error)
    json <- c(json, read_ndjson(path_error, fallback = groq_json_fallback))
  }

  ids <- as.numeric(gsub("chat-", "", map_chr(json, "[[", "custom_id")))
  results <- lapply(json, "[[", "response")
  results[order(ids)]
}

method(batch_result_turn, ProviderGroq) <- function(
  provider,
  result,
  has_type = FALSE
) {
  if (!is.null(result) && result$status_code == 200L && !is.null(result$body)) {
    value_turn(provider, result$body, has_type = has_type)
  } else {
    NULL
  }
}

# Batch helpers ----------------------------------------------------------------

groq_upload_file <- function(provider, path, purpose = "batch") {
  req <- base_request(provider)
  req <- req_url_path_append(req, "files")
  req <- req_body_multipart(
    req,
    purpose = purpose,
    file = curl::form_file(path)
  )
  req <- req_progress(req, "up")

  resp <- req_perform(req)
  resp_body_json(resp)
}

groq_download_file <- function(provider, id, path) {
  req <- base_request(provider)
  req <- req_url_path_append(req, "files", id, "content")
  req <- req_progress(req, "down")
  req_perform(req, path = path)

  invisible(path)
}

groq_json_fallback <- function(line) {
  list(
    custom_id = extract_custom_id(line),
    response = list(status_code = 500)
  )
}
