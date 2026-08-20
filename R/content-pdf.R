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
#' Both functions embed the PDF's contents in every request, so for a large
#' PDF, or one you'll refer to across several turns, prefer
#' `chat$file_upload()`. It uploads the file once and later turns reference
#' it by id.
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
    data = base64_enc(path = path),
    filename = basename(path)
  )
}

#' @rdname content_pdf_file
#' @export
content_pdf_url <- function(url) {
  if (grepl("^data:", url)) {
    parsed <- parse_data_url(url)
    ContentPDF(parsed$content_type, parsed$base64, unique_pdf_name())
  } else {
    path <- tempfile(fileext = ".pdf")
    on.exit(unlink(path))
    httr2::req_perform(httr2::request(url), path = path)

    filename <- basename(sub("[?#].*$", "", url))
    if (!grepl("\\.pdf$", filename, ignore.case = TRUE)) {
      filename <- unique_pdf_name()
    }

    ContentPDF(
      type = "application/pdf",
      data = base64_enc(path = path),
      filename = filename,
      url = url
    )
  }
}

unique_pdf_name <- function() {
  the$cur_pdf_id <- (the$cur_pdf_id %||% 0) + 1
  sprintf("file_%03d.pdf", the$cur_pdf_id)
}
