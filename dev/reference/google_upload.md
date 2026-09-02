# Upload a file to gemini

**\[deprecated\]**

This function is deprecated in favour of the provider-neutral
[Chat](https://ellmer.tidyverse.org/dev/reference/Chat.md) method
`chat$file_upload()`.

## Usage

``` r
google_upload(
  path,
  base_url = "https://generativelanguage.googleapis.com/",
  api_key = NULL,
  credentials = NULL,
  mime_type = NULL
)
```

## Arguments

- path:

  Path to a file to upload.

- base_url:

  The base URL to the API endpoint.

- api_key:

  **\[deprecated\]** Use `credentials` instead.

- credentials:

  A function that returns a list of authentication headers or `NULL`,
  the default, to use ambient credentials. See above for details.

- mime_type:

  Optionally, specify the mime type of the file. If not specified, will
  be guessed from the file extension.

## Value

A `<ContentUploaded>` object that can be passed to `$chat()`.

## Examples

``` r
if (FALSE) { # \dontrun{
chat <- chat_google_gemini()
file <- chat$file_upload("path/to/file.pdf")
chat$chat(file, "Give me a three paragraph summary of this PDF")
} # }
```
