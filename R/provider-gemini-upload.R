#' Upload a file to gemini
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This function uploads a file then waits for Gemini to finish processing it
#' so that you can immediately use it in a prompt. It's experimental because
#' it's currently Gemini specific, and we expect other providers to evolve
#' similar feature in the future.
#'
#' Uploaded files are automatically deleted after 2 days. Each file must be
#' less than 2 GB and you can upload a total of 20 GB. ellmer doesn't currently
#' provide a way to delete files early; please
#' [file an issue](https://github.com/tidyverse/ellmer/issues) if this would
#' be useful for you.
#'
#' @inheritParams chat_google_gemini
#' @param path Path to a file to upload.
#' @param mime_type Optionally, specify the mime type of the file.
#'   If not specified, will be guesses from the file extension.
#' @returns A `<ContentUploaded>` object that can be passed to `$chat()`.
#' @export
#' @examples
#' \dontrun{
#' file <- google_upload("path/to/file.pdf")
#'
#' chat <- chat_google_gemini()
#' chat$chat(file, "Give me a three paragraph summary of this PDF")
#' }
google_upload <- function(
  path,
  base_url = "https://generativelanguage.googleapis.com/v1beta/",
  api_key = NULL,
  mime_type = NULL
) {
  credentials <- default_google_credentials(api_key, gemini = TRUE)

  mime_type <- mime_type %||% guess_mime_type(path)

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

  ContentUploaded(uri = status$uri, mime_type = status$mimeType)
}

# https://ai.google.dev/api/files#method:-media.upload
google_upload_init <- function(path, base_url, credentials, mime_type) {
  file_size <- file.size(path)
  display_name <- basename(path)

  req <- request(base_url)
  req <- ellmer_req_credentials(req, credentials)
  req <- req_url_path(req, "upload/v1beta/files")
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
  req <- ellmer_req_credentials(req, credentials)
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
  req <- ellmer_req_credentials(req, credentials)

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

  invisible()
}

# Helpers ----------------------------------------------------------------------

guess_mime_type <- function(file_path, call = caller_env()) {
  ext <- tolower(tools::file_ext(file_path))

  if (has_name(mime_types, ext)) {
    mime_types[[ext]]
  } else {
    cli::cli_abort(
      c(
        "x" = "Couldn't determine mime type for {.arg path} because it has an unknown file extension, {ext}.",
        "i" = "Please supply the {.arg mime_type} manually."
      )
    )
  }
}

mime_types <- list(
  # Images
  jpg = "image/jpeg",
  jpeg = "image/jpeg",
  png = "image/png",
  gif = "image/gif",
  bmp = "image/bmp",
  svg = "image/svg+xml",
  webp = "image/webp",
  tiff = "image/tiff",
  ico = "image/x-icon",

  # Documents
  pdf = "application/pdf",
  doc = "application/msword",
  docx = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
  xls = "application/vnd.ms-excel",
  xlsx = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
  ppt = "application/vnd.ms-powerpoint",
  pptx = "application/vnd.openxmlformats-officedocument.presentationml.presentation",
  txt = "text/plain",
  rtf = "application/rtf",

  # Audio
  mp3 = "audio/mpeg",
  wav = "audio/wav",
  ogg = "audio/ogg",
  m4a = "audio/mp4",
  flac = "audio/flac",
  aac = "audio/aac",

  # Video
  mp4 = "video/mp4",
  avi = "video/x-msvideo",
  mkv = "video/x-matroska",
  mov = "video/quicktime",
  wmv = "video/x-ms-wmv",
  webm = "video/webm",

  # Web
  html = "text/html",
  htm = "text/html",
  css = "text/css",
  js = "application/javascript",
  json = "application/json",
  xml = "application/xml",

  # Data
  csv = "text/csv",
  tsv = "text/tab-separated-values",
  sql = "application/sql"
)
