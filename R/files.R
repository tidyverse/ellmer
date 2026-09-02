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
