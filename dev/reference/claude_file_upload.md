# Upload, download, and manage files for Claude

**\[deprecated\]**

These functions are deprecated in favour of the provider-neutral
[Chat](https://ellmer.tidyverse.org/dev/reference/Chat.md) methods:
`chat$file_upload()`, `chat$file_list()`, `chat$file_get()`,
`chat$file_download()`, and `chat$file_delete()`.

## Usage

``` r
claude_file_upload(
  path,
  base_url = "https://api.anthropic.com/v1/",
  beta_headers = character(),
  credentials = NULL
)

claude_file_list(
  base_url = "https://api.anthropic.com/v1/",
  credentials = NULL,
  beta_headers = character()
)

claude_file_get(
  file_id,
  base_url = "https://api.anthropic.com/v1/",
  credentials = NULL,
  beta_headers = character()
)

claude_file_download(
  file_id,
  path,
  base_url = "https://api.anthropic.com/v1/",
  credentials = NULL,
  beta_headers = character()
)

claude_file_delete(
  file_id,
  base_url = "https://api.anthropic.com/v1/",
  credentials = NULL,
  beta_headers = character()
)
```

## Arguments

- path:

  Path to download the file to.

- base_url:

  The base URL to the endpoint; the default is Claude's public API.

- beta_headers:

  Beta headers to use for the request.

- credentials:

  Override the default credentials. You generally should not need this
  argument; instead set the `ANTHROPIC_API_KEY` environment variable.
  The best place to set this is in `.Renviron`, which you can easily
  edit by calling `usethis::edit_r_environ()`.

  If you do need additional control, this argument takes a zero-argument
  function that returns either a string (the API key), or a named list
  (added as additional headers to every request).

- file_id:

  ID of the file to get information about, download, or delete.

## Examples

``` r
if (FALSE) { # \dontrun{
chat <- chat_anthropic()
file <- chat$file_upload("path/to/file.pdf")
chat$chat("Please summarize the document.", file)
} # }
```
