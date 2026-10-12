#' @include files.R
NULL

#' Encode documents for chat input
#'
#' @description
#' These functions are used to prepare text-based documents (plain text,
#' Markdown, CSV, HTML, code files, and, for providers that support them,
#' Word/Excel files) as input to the chatbot. `content_document_url()` is
#' used to provide a URL to a document, while `content_document_file()` is
#' used for local files.
#'
#' Not all providers support all document types, so check the documentation
#' for the provider you are using. For PDFs, use [content_pdf_file()] or
#' [content_pdf_url()] instead.
#'
#' Both functions embed the document's contents in every request, so for a
#' large document, or one you'll refer to across several turns, prefer
#' `chat$file_upload()`. It uploads the file once and later turns reference
#' it by id.
#'
#' @param path,url Path or URL to a document.
#' @param mime_type MIME type of the document. If not supplied, it's
#'   inferred from the file extension; unknown extensions are assumed to be
#'   plain text (e.g. code files).
#' @return A `ContentDocument` object
#' @export
content_document_file <- function(path, mime_type = NULL) {
  check_string(path, allow_empty = FALSE)
  check_string(mime_type, allow_empty = FALSE, allow_null = TRUE)
  if (!file.exists(path) || dir.exists(path)) {
    cli::cli_abort("{.arg path} must be an existing file.")
  }

  mime_type <- mime_type %||% guess_mime_type(path, default = "text/plain")
  check_not_pdf(path, mime_type)

  ContentDocument(
    mime_type = mime_type,
    data = base64_enc(path = path),
    filename = basename(path)
  )
}

#' @rdname content_document_file
#' @export
content_document_url <- function(url, mime_type = NULL) {
  check_string(url, allow_empty = FALSE)
  check_string(mime_type, allow_empty = FALSE, allow_null = TRUE)

  if (grepl("^data:", url)) {
    parsed <- parse_data_url(url)
    check_not_pdf("", parsed$content_type)

    ContentDocument(
      mime_type = parsed$content_type,
      data = parsed$base64,
      filename = unique_document_name(parsed$content_type)
    )
  } else {
    filename <- basename(sub("[?#].*$", "", url))
    mime_type <- mime_type %||%
      guess_mime_type(filename, default = "text/plain")
    check_not_pdf(filename, mime_type)
    if (tools::file_ext(filename) == "") {
      filename <- unique_document_name(mime_type)
    }

    path <- tempfile(fileext = paste0(".", tools::file_ext(filename)))
    on.exit(unlink(path))
    httr2::req_perform(httr2::request(url), path = path)

    ContentDocument(
      mime_type = mime_type,
      data = base64_enc(path = path),
      filename = filename,
      url = url
    )
  }
}

# Text-based documents can be sent as text to providers that can't extract
# text from binary formats like docx or pptx themselves
is_text_document <- function(mime_type) {
  grepl("^text/", mime_type) ||
    mime_type %in% c("application/json", "application/xml")
}

check_not_pdf <- function(filename, mime_type, error_call = caller_env()) {
  # Strip any parameters (e.g. "; charset=binary") before comparing
  mime_type <- tolower(sub(";.*$", "", mime_type))
  if (
    tolower(tools::file_ext(filename)) == "pdf" ||
      identical(trimws(mime_type), "application/pdf")
  ) {
    cli::cli_abort(
      c(
        "Documents can't be PDFs.",
        i = "Use {.fn content_pdf_file} or {.fn content_pdf_url} instead."
      ),
      call = error_call
    )
  }
}

unique_document_name <- function(mime_type) {
  the$cur_document_id <- (the$cur_document_id %||% 0) + 1
  ext <- names(mime_types)[match(mime_type, mime_types)]
  if (is.na(ext)) {
    ext <- "txt"
  }
  sprintf("document_%03d.%s", the$cur_document_id, ext)
}
