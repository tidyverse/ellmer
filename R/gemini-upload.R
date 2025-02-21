#' Upload a file to gemini
#'
#' @description
#' This function uploads a file then waits for Gemini to finish processing it
#' so that you can immediately use it in a prompt.
#'
#' Uploaded files are automatically deleted after 2 days. Each file must be
#' less than 2 GB and you can upload a total of 20 GB. ellmer doesn't currently
#' provide a way to delete files early; please
#' [file an issue](https://github.com/tidyverse/ellmer/issues) if this would
#' be useful for you.
#'
#' @inheritParams chat_gemini
#' @param path Path to a file to upload.
#' @param mime_type Optionally, specify the mime type of the file.
#'   If not specified, will be guesses from the file extension.
#' @returns A `<ContentUploaded>` object that can be passed to `$chat()`.
#' @export
#' @examples
#' \dontrun{
#' file <- gemini_upload("path/to/file.pdf")
#'
#' chat <- chat_openai()
#' chat$chat(file, "Give me a three paragraph summary of this PDF")
#' }
gemini_upload <- function(
  path,
  base_url = "https://generativelanguage.googleapis.com/v1beta/",
  api_key = Sys.getenv("GOOGLE_API_KEY"),
  mime_type = NULL
) {
  mime_type <- mime_type %||% guess_mime_type(path)

  upload_url <- gemini_upload_init(
    path = path,
    base_url = base_url,
    api_key = api_key,
    mime_type = mime_type
  )

  status <- gemini_upload_send(
    upload_url = upload_url,
    path = path,
    api_key = api_key
  )

  cli::cli_progress_bar(format = "{cli::pb_spin} Processing [{cli::pb_elapsed}] ")
  while (status$state == "PROCESSING") {
    cli::cli_progress_update()
    status <- gemini_upload_status(status$uri, api_key)
    Sys.sleep(0.5)
  }
  if (status$state == "FAILED") {
    cli::cli_abort("Upload failed: {status$error$message}")
  }

  ContentUploaded(uri = status$uri, mime_type = status$mimeType)
}

# https://ai.google.dev/api/files#method:-media.upload
gemini_upload_init <- function(path, base_url, api_key, mime_type) {
  file_size <- file.size(path)
  display_name <- fs::path_file(path)

  req <- request(base_url) |>
  req_url_path("upload/v1beta/files") |>
  req_headers_redacted("x-goog-api-key" = api_key) |>
  req_headers(
    "X-Goog-Upload-Protocol" = "resumable",
    "X-Goog-Upload-Command" = "start",
    "X-Goog-Upload-Header-Content-Length" = toString(file_size),
    "X-Goog-Upload-Header-Content-Type" = mime_type,
  ) |>
  req_body_json(list(file = list(display_name = display_name)))

  resp <- req_perform(req)
  resp_header(resp, "x-goog-upload-url")
}

gemini_upload_send <- function(upload_url, path, api_key) {
  file_size <- file.size(path)

  req <- request(upload_url) |>
  req_headers_redacted("x-goog-api-key" = api_key) |>
    req_headers(
      "Content-Length" = toString(file_size),
      "X-Goog-Upload-Offset" = "0",
      "X-Goog-Upload-Command" = "upload, finalize"
    ) |>
    req_body_file(path) |>
    req_progress("up")
  resp <- req_perform(req)
  resp_body_json(resp)$file
}

gemini_upload_status <- function(uri, api_key) {
  resp <- req_perform(request(uri) |>
    req_headers_redacted("x-goog-api-key" = api_key))
  resp_body_json(resp)
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
