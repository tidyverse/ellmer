#' @include provider.R
#' @include content.R
NULL

file_upload <- new_generic(
  "file_upload",
  "provider",
  function(provider, path, mime_type = NULL, expires_in_h = 48, ...) {
    S7_dispatch()
  }
)
method(file_upload, Provider) <- function(
  provider,
  path,
  mime_type = NULL,
  expires_in_h = 48,
  ...
) {
  no_file_support(provider)
}

file_list <- new_generic("file_list", "provider", function(provider, ...) {
  S7_dispatch()
})
method(file_list, Provider) <- function(provider, ...) {
  no_file_support(provider)
}

file_get <- new_generic("file_get", "provider", function(provider, id, ...) {
  S7_dispatch()
})
method(file_get, Provider) <- function(provider, id, ...) {
  no_file_support(provider)
}

file_download <- new_generic(
  "file_download",
  "provider",
  function(provider, id, path, ...) {
    S7_dispatch()
  }
)
method(file_download, Provider) <- function(provider, id, path, ...) {
  no_file_support(provider)
}

file_delete <- new_generic(
  "file_delete",
  "provider",
  function(provider, id, ...) {
    S7_dispatch()
  }
)
method(file_delete, Provider) <- function(provider, id, ...) {
  no_file_support(provider)
}

no_file_support <- function(provider, call = caller_env()) {
  cli::cli_abort(
    c(
      "{provider@name} doesn't support file management.",
      i = "File management is supported by {.fn chat_openai}, {.fn chat_anthropic}, and {.fn chat_google_gemini}."
    ),
    class = "not_implemented",
    call = call
  )
}

check_upload_path <- function(path, error_call = caller_env()) {
  check_string(path, allow_empty = FALSE, call = error_call)
  if (!file.exists(path)) {
    cli::cli_abort("{.arg path} must be an existing file.", call = error_call)
  }
}

# `Inf` means never expire
check_expires_in <- function(expires_in_h, max, error_call = caller_env()) {
  check_number_decimal(
    expires_in_h,
    min = 1,
    max = max,
    allow_infinite = TRUE,
    call = error_call
  )
}

# Providers take expiry as an integer number of seconds in a form field, and
# as.character(100000) gives "1e+05"
hours_to_seconds <- function(x) {
  format(round(x * 60 * 60), scientific = FALSE, trim = TRUE)
}

as_file_id <- function(id, error_call = caller_env()) {
  if (S7_inherits(id, ContentUploaded)) {
    id@uri
  } else {
    check_string(id, allow_empty = FALSE, call = error_call)
    id
  }
}

# Parse RFC3339 timestamps like "2026-08-15T11:11:23.914838Z"; bare
# as.POSIXct() silently drops the time component and uses the local timezone.
parse_rfc3339 <- function(x) {
  as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
}

form_file <- function(path, type = type) {
  curl::form_file(path, type = type)
}

# Extension -> MIME type lookup shared by inline content helpers and
# provider file uploads, so a given file gets the same type either way.
guess_mime_type <- function(path, default = NULL, call = caller_env()) {
  ext <- tolower(tools::file_ext(path))

  if (has_name(mime_types, ext)) {
    mime_types[[ext]]
  } else if (!is.null(default)) {
    default
  } else {
    cli::cli_abort(
      c(
        "x" = "Couldn't determine mime type for {.arg path} because it has an unknown file extension, {ext}.",
        "i" = "Please supply the {.arg mime_type} manually."
      ),
      call = call
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
  md = "text/markdown",
  markdown = "text/markdown",
  odt = "application/vnd.oasis.opendocument.text",
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
