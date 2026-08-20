# pdfs are redirected to content_pdf_ functions

    Code
      content_document_file(path)
    Condition
      Error in `content_document_file()`:
      ! Documents can't be PDFs.
      i Use `content_pdf_file()` or `content_pdf_url()` instead.
    Code
      content_document_url("https://example.com/report.pdf")
    Condition
      Error in `content_document_url()`:
      ! Documents can't be PDFs.
      i Use `content_pdf_file()` or `content_pdf_url()` instead.
    Code
      content_document_file(test_path("penguin_race.csv"), mime_type = "application/pdf")
    Condition
      Error in `content_document_file()`:
      ! Documents can't be PDFs.
      i Use `content_pdf_file()` or `content_pdf_url()` instead.

# errors if file doesn't exist

    Code
      content_document_file("DOESNTEXIST")
    Condition
      Error in `content_document_file()`:
      ! `path` must be an existing file.

