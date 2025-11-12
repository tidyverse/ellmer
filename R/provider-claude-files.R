#' Upload, downloand, and manage files for Claude
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' Use the beta Files API to upload files to and manage files in Claude.
#' This is currently experimental because the API is in beta and may change.
#' Note that you need `beta-headers = "files-api-2025-04-14"` to use the API.
#'
#' Claude offers 100GB of file storage per organization, with each file
#' having a maximum size of 500MB. For more details see
#' <https://docs.claude.com/en/docs/build-with-claude/files>
#'
#' * `claude_file_upload()` uploads a file and returns an object that
#'   you can use in chat.
#' * `claude_file_list()` lists all uploaded files.
#' * `claude_file_get()` returns an object for an previously uploaded file.
#' * `claude_file_download()` downloads the file with the given ID. Note
#'   that you can only download files created by skills or the code execution
#'   tool.
#' * `claude_file_delete()` deletes the file with the given ID.
#'
#' @inheritParams chat_anthropic
#' @param path Path to a file to upload.
#' @param file_id ID of the file to get information about, download, or delete.
#' @param beta_headers Beta headers to use for the request. Defaults to
#'   `files-api-2025-04-14`.
#' @export
#' @examples
#' \dontrun{
#' file <- claude_file_upload("path/to/file.pdf")
#' chat <- chat_anthropic(beta_headers = "files-api-2025-04-14")
#' chat$chat("Please summarize the document.", file)
#' }

claude_file_upload <- function(
  path,
  base_url = "https://api.anthropic.com/v1/",
  beta_headers = "files-api-2025-04-14",
  credentials = NULL
) {
  check_string(path, allow_empty = FALSE)
  if (!file.exists(path)) {
    cli::cli_abort("{.arg path} must be an existing file.")
  }
  file <- form_file(path, type = guess_mime_type(path))

  req <- request_anthropic_file(base_url, beta_headers, credentials)
  req <- req_url_path_append(req, "/files")
  req <- req_body_multipart(req, file = file)
  resp <- req_perform(req)
  json <- resp_body_json(resp)

  ContentUploaded(uri = json$id, mime_type = json$mime_type)
}

#' @export
#' @rdname claude_file_upload
claude_file_list <- function(
  base_url = "https://api.anthropic.com/v1/",
  credentials = NULL,
  beta_headers = "files-api-2025-04-14"
) {
  req <- request_anthropic_file(base_url, beta_headers, credentials)
  req <- req_url_path_append(req, "/files")
  resp <- req_perform(req)

  data <- resp_body_json(resp)$data
  data.frame(
    id = map_chr(data, "[[", "id"),
    filename = map_chr(data, "[[", "filename"),
    mime_type = map_chr(data, "[[", "mime_type"),
    size = map_dbl(data, "[[", "size_bytes"),
    created_at = map_chr(data, "[[", "created_at")
  )
}

#' @export
#' @rdname claude_file_upload
claude_file_get <- function(
  file_id,
  base_url = "https://api.anthropic.com/v1/",
  credentials = NULL,
  beta_headers = "files-api-2025-04-14"
) {
  req <- request_anthropic_file(base_url, beta_headers, credentials)
  req <- req_url_path_append(req, "files", file_id)
  resp <- req_perform(req)
  json <- resp_body_json(resp)

  ContentUploaded(uri = json$id, mime_type = json$mime_type)
}

#' @export
#' @rdname claude_file_upload
#' @param path Path to download the file to.
claude_file_download <- function(
  file_id,
  path,
  base_url = "https://api.anthropic.com/v1/",
  credentials = NULL,
  beta_headers = "files-api-2025-04-14"
) {
  check_string(path)

  req <- request_anthropic_file(base_url, beta_headers, credentials)
  req <- req_url_path_append(req, "files", file_id, "content")
  req_perform(req, path = path)

  invisible(path)
}

#' @export
#' @rdname claude_file_upload
claude_file_delete <- function(
  file_id,
  base_url = "https://api.anthropic.com/v1/",
  credentials = NULL,
  beta_headers = "files-api-2025-04-14"
) {
  req <- request_anthropic_file(base_url, beta_headers, credentials)
  req <- req_url_path_append(req, "files", file_id)
  req <- req_method(req, "DELETE")
  resp <- req_perform(req)

  invisible()
}

request_anthropic_file <- function(url, beta_headers, credentials) {
  credentials <- as_credentials(
    "chat_anthropic",
    function() anthropic_key(),
    credentials = credentials
  )

  provider <- ProviderAnthropic(
    name = "Anthropic",
    model = "",
    base_url = url,
    params = list(),
    extra_args = list(),
    credentials = credentials,
    beta_headers = beta_headers,
    cache = "none"
  )

  base_request(provider)
}

form_file <- function(path, type = type) {
  curl::form_file(path, type = type)
}
