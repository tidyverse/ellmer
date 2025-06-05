#' Encode PDFs content for chat input
#'
#' @description
#' These functions are used to prepare PDFs as input to the chatbot. The
#' `content_pdf_url()` function is used to provide a URL to an PDF file,
#' while `content_pdf_file()` is used to for local PDF files.
#'
#' Not all providers support PDF input, so check the documentation for the
#' provider you are using.
#'
#' @param path,url Path or URL to a PDF file.
#' @return A `ContentPDF` object
#' @export
content_pdf_file <- function(path) {
  check_string(path, allow_empty = FALSE)
  if (!file.exists(path) || dir.exists(path)) {
    cli::cli_abort("{.arg path} must be an existing file.")
  }

  ContentPDF(
    type = "application/pdf",
    data = base64_enc(path = path)
  )
}

#' @rdname content_pdf_file
#' @export
content_pdf_url <- function(url) {
  if (grepl("^data:", url)) {
    parsed <- parse_data_url(url)
    ContentPDF(parsed$content_type, parsed$base64)
  } else {
    # TODO: need seperate ContentPDFRemote type so we can use file upload
    # apis where they exist. Might need some kind of mutable state so can
    # record point to uploaded file.
    path <- tempfile(fileext = ".pdf")
    on.exit(unlink(path))

    resp <- httr2::req_perform(httr2::request(url), path = path)
    content_pdf_file(path)
  }
}
