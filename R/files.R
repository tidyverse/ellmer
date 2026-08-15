#' @include provider.R
#' @include content.R
NULL

file_upload <- new_generic(
  "file_upload",
  "provider",
  function(provider, path, mime_type = NULL, ...) {
    S7_dispatch()
  }
)
method(file_upload, Provider) <- function(
  provider,
  path,
  mime_type = NULL,
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

as_file_id <- function(id, error_call = caller_env()) {
  if (S7_inherits(id, ContentUploaded)) {
    id@uri
  } else {
    check_string(id, allow_empty = FALSE, call = error_call)
    id
  }
}

has_uploaded_content <- function(turns) {
  some(turns, function(turn) {
    some(turn@contents, function(content) S7_inherits(content, ContentUploaded))
  })
}

form_file <- function(path, type = type) {
  curl::form_file(path, type = type)
}
