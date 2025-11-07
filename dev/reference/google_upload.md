# Upload a file to gemini

**\[experimental\]**

This function uploads a file then waits for Gemini to finish processing
it so that you can immediately use it in a prompt. It's experimental
because it's currently Gemini specific, and we expect other providers to
evolve similar feature in the future.

Uploaded files are automatically deleted after 2 days. Each file must be
less than 2 GB and you can upload a total of 20 GB. ellmer doesn't
currently provide a way to delete files early; please [file an
issue](https://github.com/tidyverse/ellmer/issues) if this would be
useful for you.

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

  The base URL to the endpoint; the default is OpenAI's public API.

- api_key:

  **\[deprecated\]** Use `credentials` instead.

- credentials:

  A function that returns a list of authentication headers or `NULL`,
  the default, to use ambient credentials. See above for details.

- mime_type:

  Optionally, specify the mime type of the file. If not specified, will
  be guesses from the file extension.

## Value

A `<ContentUploaded>` object that can be passed to `$chat()`.

## Examples

``` r
if (FALSE) { # \dontrun{
file <- google_upload("path/to/file.pdf")

chat <- chat_google_gemini()
chat$chat(file, "Give me a three paragraph summary of this PDF")
} # }
```
