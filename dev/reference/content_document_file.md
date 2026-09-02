# Encode documents for chat input

These functions are used to prepare text-based documents (plain text,
Markdown, CSV, HTML, code files, and, for providers that support them,
Word/Excel files) as input to the chatbot. `content_document_url()` is
used to provide a URL to a document, while `content_document_file()` is
used for local files.

Not all providers support all document types, so check the documentation
for the provider you are using. For PDFs, use
[`content_pdf_file()`](https://ellmer.tidyverse.org/dev/reference/content_pdf_file.md)
or
[`content_pdf_url()`](https://ellmer.tidyverse.org/dev/reference/content_pdf_file.md)
instead.

Both functions embed the document's contents in every request, so for a
large document, or one you'll refer to across several turns, prefer
`chat$file_upload()`. It uploads the file once and later turns reference
it by id.

## Usage

``` r
content_document_file(path, mime_type = NULL)

content_document_url(url, mime_type = NULL)
```

## Arguments

- path, url:

  Path or URL to a document.

- mime_type:

  MIME type of the document. If not supplied, it's inferred from the
  file extension; unknown extensions are assumed to be plain text (e.g.
  code files).

## Value

A `ContentDocument` object
