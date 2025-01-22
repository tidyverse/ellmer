#' Create PDF content from a file or URL
#'
#' These functions are used to prepare PDFs as input to the
#' chatbot. The `content_pdf_url()` function is used to provide a URL to an
#' PDF file, while `content_pdf_file()` is used to for local PDF files.  Not all providers
#' support PDF input, so check the documentation for the provider you are using.
#'
#' @param path Path to a PDF file
#' @param compress Whether to compress the PDF before sending. Requires the qpdf package to be installed
#' @return A ContentPDF object
#' @export
content_pdf_file <- function(path, compress = FALSE) {
  check_string(path, allow_empty = FALSE)

  if (!file.exists(path) || dir.exists(path)) {
    cli::cli_abort("{.arg path} must be an existing file.")
  }

  if (isTRUE(compress)) {
    check_installed("qpdf", "to compress PDFs")

    temp_file <- tempfile(fileext = ".pdf")
    on.exit(unlink(temp_file))

    qpdf::pdf_compress(path, temp_file)
    path <- temp_file
  }

  # Base64 encode the PDF
  base64 <- base64enc::base64encode(path)

  ContentPDF(
    type = "application/pdf",
    data = base64
  )
}

#' @rdname content_pdf_file
#' @export
content_pdf_url <- function(url, compress = FALSE) {
  if (grepl("^data:", url)) {
    parts <- strsplit(sub("^data:", "", url), ";")[[1]]
    if (length(parts) != 2 || parts[2] != "base64") {
      cli::cli_abort("Invalid data URL format")
    }

    ContentPDF(
      type = parts[1],
      data = sub("^base64,", "", parts[2])
    )
  } else {
    # Download the PDF and convert to base64
    resp <- httr2::request(url) |>
      httr2::req_perform()

    # Check if it's actually a PDF
    content_type <- httr2::resp_content_type(resp)
    if (!identical(content_type, "application/pdf")) {
      cli::cli_abort("URL does not point to a PDF (content-type: {content_type})")
    }

    # Save to temp file
    temp_file <- tempfile(fileext = ".pdf")
    on.exit(unlink(temp_file))
    writeBin(httr2::resp_body_raw(resp), temp_file)

    if (isTRUE(compress)) {
      check_installed("qpdf", "to compress PDFs")

      compressed_file <- tempfile(fileext = ".pdf")
      on.exit(unlink(compressed_file), add = TRUE)

      qpdf::pdf_compress(temp_file, compressed_file)
      temp_file <- compressed_file
    }

    # Base64 encode the final file
    base64 <- base64enc::base64encode(temp_file)

    ContentPDF(
      type = "application/pdf",
      data = base64
    )
  }
}
