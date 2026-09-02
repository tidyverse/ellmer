#' @include provider-claude.R
#' @include files.R
NULL

method(file_upload, ProviderAnthropic) <- function(
  provider,
  path,
  mime_type = NULL,
  expires_in_h = 48,
  ...
) {
  check_upload_path(path)
  check_expires_in(expires_in_h, max = 90 * 24)
  mime_type <- mime_type %||% guess_mime_type(path)

  req <- anthropic_files_request(provider)
  req <- req_body_multipart(req, file = form_file(path, type = mime_type))
  if (is.finite(expires_in_h)) {
    req <- req_body_multipart(
      req,
      expires_in_seconds = hours_to_seconds(expires_in_h)
    )
  }
  resp <- req_perform(req)
  json <- resp_body_json(resp)

  ContentUploaded(
    uri = json$id,
    mime_type = mime_type,
    provider = "anthropic",
    extra = list(filename = json$filename, size_bytes = json$size_bytes)
  )
}

method(file_list, ProviderAnthropic) <- function(provider, ...) {
  data <- list()
  page <- NULL
  repeat {
    req <- anthropic_files_request(provider)
    req <- req_url_query(req, page = page)
    json <- resp_body_json(req_perform(req))
    data <- c(data, json$data)
    page <- json$next_page
    if (is.null(page)) {
      break
    }
  }

  data.frame(
    id = map_chr(data, "[[", "id"),
    filename = map_chr(data, "[[", "filename"),
    mime_type = map_chr(data, "[[", "mime_type"),
    size_bytes = map_dbl(data, "[[", "size_bytes"),
    created_at = parse_rfc3339(map_chr(data, "[[", "created_at")),
    expires_at = parse_rfc3339(map_chr(data, function(file) {
      file$expires_at %||% NA_character_
    })),
    downloadable = map_lgl(data, "[[", "downloadable")
  )
}

method(file_get, ProviderAnthropic) <- function(provider, id, ...) {
  req <- anthropic_files_request(provider)
  req <- req_url_path_append(req, as_file_id(id))
  json <- resp_body_json(req_perform(req))

  ContentUploaded(
    uri = json$id,
    mime_type = json$mime_type,
    provider = "anthropic",
    extra = list(
      filename = json$filename,
      size_bytes = as.numeric(json$size_bytes),
      created_at = parse_rfc3339(json$created_at),
      expires_at = parse_rfc3339(json$expires_at %||% NA_character_),
      downloadable = json$downloadable
    )
  )
}

method(file_download, ProviderAnthropic) <- function(provider, id, path, ...) {
  check_string(path)

  req <- anthropic_files_request(provider)
  req <- req_url_path_append(req, as_file_id(id), "content")
  req_perform(req, path = path)

  invisible(path)
}

method(file_delete, ProviderAnthropic) <- function(provider, id, ...) {
  req <- anthropic_files_request(provider)
  req <- req_url_path_append(req, as_file_id(id))
  req <- req_method(req, "DELETE")
  req_perform(req)

  invisible()
}

# https://platform.claude.com/docs/en/build-with-claude/files
anthropic_files_request <- function(provider) {
  req <- base_request(provider)
  req_url_path_append(req, "/files")
}

#' Upload, download, and manage files for Claude
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' These functions are deprecated in favour of the provider-neutral [Chat]
#' methods: `chat$file_upload()`, `chat$file_list()`, `chat$file_get()`,
#' `chat$file_download()`, and `chat$file_delete()`.
#'
#' @inheritParams chat_anthropic
#' @param base_url The base URL to the endpoint; the default is Claude's
#'   public API.
#' @param path Path to a file to upload.
#' @param file_id ID of the file to get information about, download, or delete.
#' @param beta_headers Beta headers to use for the request.
#' @export
#' @examples
#' \dontrun{
#' chat <- chat_anthropic()
#' file <- chat$file_upload("path/to/file.pdf")
#' chat$chat("Please summarize the document.", file)
#' }
claude_file_upload <- function(
  path,
  base_url = "https://api.anthropic.com/v1/",
  beta_headers = character(),
  credentials = NULL
) {
  lifecycle::deprecate_warn(
    "0.5.0",
    "claude_file_upload()",
    "Chat$file_upload()"
  )
  provider <- anthropic_file_provider(base_url, beta_headers, credentials)
  file_upload(provider, path, expires_in_h = Inf)
}

#' @export
#' @rdname claude_file_upload
claude_file_list <- function(
  base_url = "https://api.anthropic.com/v1/",
  credentials = NULL,
  beta_headers = character()
) {
  lifecycle::deprecate_warn("0.5.0", "claude_file_list()", "Chat$file_list()")
  provider <- anthropic_file_provider(base_url, beta_headers, credentials)
  file_list(provider)
}

#' @export
#' @rdname claude_file_upload
claude_file_get <- function(
  file_id,
  base_url = "https://api.anthropic.com/v1/",
  credentials = NULL,
  beta_headers = character()
) {
  lifecycle::deprecate_warn("0.5.0", "claude_file_get()", "Chat$file_get()")
  provider <- anthropic_file_provider(base_url, beta_headers, credentials)
  file_get(provider, file_id)
}

#' @export
#' @rdname claude_file_upload
#' @param path Path to download the file to.
claude_file_download <- function(
  file_id,
  path,
  base_url = "https://api.anthropic.com/v1/",
  credentials = NULL,
  beta_headers = character()
) {
  lifecycle::deprecate_warn(
    "0.5.0",
    "claude_file_download()",
    "Chat$file_download()"
  )
  provider <- anthropic_file_provider(base_url, beta_headers, credentials)
  file_download(provider, file_id, path)
}

#' @export
#' @rdname claude_file_upload
claude_file_delete <- function(
  file_id,
  base_url = "https://api.anthropic.com/v1/",
  credentials = NULL,
  beta_headers = character()
) {
  lifecycle::deprecate_warn(
    "0.5.0",
    "claude_file_delete()",
    "Chat$file_delete()"
  )
  provider <- anthropic_file_provider(base_url, beta_headers, credentials)
  file_delete(provider, file_id)
}

anthropic_file_provider <- function(base_url, beta_headers, credentials) {
  credentials <- as_credentials(
    "chat_anthropic",
    function() anthropic_key(),
    credentials = credentials
  )

  ProviderAnthropic(
    name = "Anthropic",
    base_url = base_url,
    credentials = credentials,
    beta_headers = beta_headers,
    cache = "none"
  )
}
