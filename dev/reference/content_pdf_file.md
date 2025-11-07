# Encode PDFs content for chat input

These functions are used to prepare PDFs as input to the chatbot. The
`content_pdf_url()` function is used to provide a URL to an PDF file,
while `content_pdf_file()` is used to for local PDF files.

Not all providers support PDF input, so check the documentation for the
provider you are using.

## Usage

``` r
content_pdf_file(path)

content_pdf_url(url)
```

## Arguments

- path, url:

  Path or URL to a PDF file.

## Value

A `ContentPDF` object
