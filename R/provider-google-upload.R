#' @include provider-google.R
#' @include files.R
NULL

#' Upload a file to gemini
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated in favour of the provider-neutral [Chat]
#' method `chat$file_upload()`.
#'
#' @inheritParams chat_google_gemini
#' @param path Path to a file to upload.
#' @param mime_type Optionally, specify the mime type of the file.
#'   If not specified, will be guessed from the file extension.
#' @returns A `<ContentUploaded>` object that can be passed to `$chat()`.
#' @export
#' @examples
#' \dontrun{
#' chat <- chat_google_gemini()
#' file <- chat$file_upload("path/to/file.pdf")
#' chat$chat(file, "Give me a three paragraph summary of this PDF")
#' }
google_upload <- function(
  path,
  base_url = "https://generativelanguage.googleapis.com/",
  api_key = NULL,
  credentials = NULL,
  mime_type = NULL
) {
  lifecycle::deprecate_warn("0.5.0", "google_upload()", "Chat$file_upload()")

  credentials <- as_credentials(
    "google_upload",
    default_google_credentials(variant = "gemini"),
    credentials = credentials,
    api_key = api_key
  )

  provider <- ProviderGoogleGemini(
    name = "Google/Gemini",
    base_url = paste0(base_url, "v1beta/"),
    credentials = credentials
  )
  file_upload(provider, path, mime_type = mime_type)
}

google_upload_file <- function(path, base_url, credentials, mime_type) {
  upload_url <- google_upload_init(
    path = path,
    base_url = base_url,
    credentials = credentials,
    mime_type = mime_type
  )

  status <- google_upload_send(
    upload_url = upload_url,
    path = path,
    credentials = credentials
  )
  google_upload_wait(status, credentials)
}

# https://ai.google.dev/api/files#method:-media.upload
google_upload_init <- function(path, base_url, credentials, mime_type) {
  file_size <- file.size(path)
  display_name <- basename(path)

  req <- request(base_url)
  req <- ellmer_req_credentials(req, credentials(), "x-goog-api-key")
  req <- req_url_path_append(req, "upload/v1beta/files")
  req <- req_headers(
    req,
    "X-Goog-Upload-Protocol" = "resumable",
    "X-Goog-Upload-Command" = "start",
    "X-Goog-Upload-Header-Content-Length" = toString(file_size),
    "X-Goog-Upload-Header-Content-Type" = mime_type,
  )
  req <- req_body_json(req, list(file = list(display_name = display_name)))

  resp <- req_perform(req)
  resp_header(resp, "x-goog-upload-url")
}

google_upload_send <- function(upload_url, path, credentials) {
  file_size <- file.size(path)

  req <- request(upload_url)
  req <- ellmer_req_credentials(req, credentials(), "x-goog-api-key")
  req <- req_headers(
    req,
    "Content-Length" = toString(file_size),
    "X-Goog-Upload-Offset" = "0",
    "X-Goog-Upload-Command" = "upload, finalize"
  )
  req <- req_body_file(req, path)
  req <- req_progress(req, "up")

  resp <- req_perform(req)
  resp_body_json(resp)$file
}

google_upload_status <- function(uri, credentials) {
  req <- request(uri)
  req <- ellmer_req_credentials(req, credentials(), "x-goog-api-key")

  resp <- req_perform(req)
  resp_body_json(resp)
}

google_upload_wait <- function(status, credentials) {
  cli::cli_progress_bar(
    format = "{cli::pb_spin} Processing [{cli::pb_elapsed}] "
  )

  while (status$state == "PROCESSING") {
    cli::cli_progress_update()
    status <- google_upload_status(status$uri, credentials)
    Sys.sleep(0.5)
  }
  if (status$state == "FAILED") {
    cli::cli_abort("Upload failed: {status$error$message}")
  }

  status
}

# Batch file helpers -----------------------------------------------------------

gemini_upload_file <- function(
  provider,
  path,
  mime_type = "application/jsonl"
) {
  upload_base_url <- sub("/v[^/]+/?$", "/", provider@base_url)

  google_upload_file(
    path = path,
    base_url = upload_base_url,
    credentials = provider@credentials,
    mime_type = mime_type
  )
}

gemini_download_file <- function(provider, name, path) {
  req <- base_request(provider)
  req <- req_url_path_append(req, paste0(name, ":download"))
  req <- req_url_query(req, alt = "media")
  req_perform(req, path = path)
  invisible(path)
}

# File management --------------------------------------------------------------

method(file_upload, ProviderGoogleGemini) <- function(
  provider,
  path,
  mime_type = NULL,
  expires_in_h = 48,
  ...
) {
  check_gemini_files_api(provider)
  check_upload_path(path)
  if (!isTRUE(expires_in_h == 48)) {
    cli::cli_abort(
      "Gemini files always expire after 48 hours, so {.arg expires_in_h} must be 48."
    )
  }
  mime_type <- mime_type %||% guess_mime_type(path)

  status <- gemini_upload_file(provider, path, mime_type = mime_type)
  if (is.null(status$uri)) {
    cli::cli_abort("Upload of {.path {path}} didn't return a URI.")
  }

  ContentUploaded(
    uri = status$uri,
    mime_type = status$mimeType %||% mime_type,
    provider = "google",
    extra = list(
      name = status$name,
      size_bytes = as.numeric(status$sizeBytes),
      state = status$state
    )
  )
}

method(file_list, ProviderGoogleGemini) <- function(provider, ...) {
  check_gemini_files_api(provider)

  data <- list()
  page_token <- NULL
  repeat {
    req <- base_request(provider)
    req <- req_url_path_append(req, "files")
    req <- req_url_query(req, pageSize = 100, pageToken = page_token)
    json <- resp_body_json(req_perform(req))
    data <- c(data, json$files)
    page_token <- json$nextPageToken
    if (is.null(page_token)) {
      break
    }
  }

  data.frame(
    id = map_chr(data, function(file) file$uri %||% file$name),
    filename = map_chr(data, function(file) {
      file$displayName %||% NA_character_
    }),
    mime_type = map_chr(data, function(file) file$mimeType %||% NA_character_),
    size_bytes = as.numeric(map_chr(data, "[[", "sizeBytes")),
    created_at = parse_rfc3339(map_chr(data, "[[", "createTime")),
    expires_at = parse_rfc3339(map_chr(data, "[[", "expirationTime")),
    name = map_chr(data, "[[", "name"),
    state = map_chr(data, "[[", "state")
  )
}

method(file_get, ProviderGoogleGemini) <- function(provider, id, ...) {
  check_gemini_files_api(provider)

  req <- base_request(provider)
  req <- req_url_path_append(req, google_file_name(id))
  json <- resp_body_json(req_perform(req))

  list(
    id = json$uri %||% json$name,
    filename = json$displayName,
    mime_type = json$mimeType,
    size_bytes = as.numeric(json$sizeBytes),
    created_at = parse_rfc3339(json$createTime),
    expires_at = parse_rfc3339(json$expirationTime),
    name = json$name,
    state = json$state
  )
}

method(file_download, ProviderGoogleGemini) <- function(
  provider,
  id,
  path,
  ...
) {
  check_gemini_files_api(provider)
  check_string(path)

  # Gemini only serves bytes back for model-generated files (e.g. video
  # output); files uploaded with file_upload() aren't downloadable.
  gemini_download_file(provider, google_file_name(id), path)
}

method(file_delete, ProviderGoogleGemini) <- function(provider, id, ...) {
  check_gemini_files_api(provider)

  req <- base_request(provider)
  req <- req_url_path_append(req, google_file_name(id))
  req <- req_method(req, "DELETE")
  req_perform(req)

  invisible()
}

check_gemini_files_api <- function(provider, call = caller_env()) {
  if (grepl("aiplatform.googleapis.com", provider@base_url, fixed = TRUE)) {
    cli::cli_abort(
      c(
        "The Gemini Files API is not available on Vertex AI.",
        i = "Upload the file to a Cloud Storage bucket and reference it with
             {.code ContentUploaded(uri = \"gs://bucket/object\", mime_type = ...)}."
      ),
      class = "not_implemented",
      call = call
    )
  }
}

# Accept a ContentUploaded, a full URI (https://.../v1beta/files/abc), a
# resource name (files/abc), or a bare id (abc), and return the resource name.
google_file_name <- function(id) {
  id <- as_file_id(id)
  id <- sub("^https?://[^/]+/v[^/]+/", "", id)
  if (!grepl("^files/", id)) {
    id <- paste0("files/", id)
  }
  id
}
