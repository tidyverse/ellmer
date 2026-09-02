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
#' @param mime_type MIME type of the document. The default, `"auto"`,
#'   infers the type from the file extension; unknown extensions are
#'   assumed to be plain text (e.g. code files).
#' @return A `ContentDocument` object
#' @export
content_document_file <- function(path, mime_type = "auto") {
  check_string(path, allow_empty = FALSE)
  check_string(mime_type, allow_empty = FALSE)
  if (!file.exists(path) || dir.exists(path)) {
    cli::cli_abort("{.arg path} must be an existing file.")
  }

  if (mime_type == "auto") {
    mime_type <- document_mime_type(path)
  }
  check_not_pdf(path, mime_type)

  ContentDocument(
    mime_type = mime_type,
    data = base64_enc(path = path),
    filename = basename(path)
  )
}

#' @rdname content_document_file
#' @export
content_document_url <- function(url, mime_type = "auto") {
  check_string(url, allow_empty = FALSE)
  check_string(mime_type, allow_empty = FALSE)

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
    if (mime_type == "auto") {
      mime_type <- document_mime_type(filename)
    }
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

# A small curated set of extensions that the major providers document as
# first-class document input. Anything else is assumed to be source code or
# another text format and defaults to "text/plain"; binary formats must be
# listed explicitly since that default would otherwise mislabel them.
document_mime_types <- c(
  txt = "text/plain",
  md = "text/markdown",
  markdown = "text/markdown",
  csv = "text/csv",
  tsv = "text/tab-separated-values",
  json = "application/json",
  html = "text/html",
  htm = "text/html",
  xml = "text/xml",
  docx = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
  xlsx = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
  rtf = "application/rtf",
  doc = "application/msword",
  odt = "application/vnd.oasis.opendocument.text",
  xls = "application/vnd.ms-excel"
)

# Binary formats that only OpenAI can extract text from
binary_document_mime_types <- unname(
  document_mime_types[c("docx", "xlsx", "rtf", "doc", "odt", "xls")]
)

document_mime_type <- function(name) {
  ext <- tolower(tools::file_ext(name))
  if (has_name(document_mime_types, ext)) {
    document_mime_types[[ext]]
  } else {
    "text/plain"
  }
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
  ext <- names(document_mime_types)[match(mime_type, document_mime_types)]
  if (is.na(ext)) {
    ext <- "txt"
  }
  sprintf("document_%03d.%s", the$cur_document_id, ext)
}
