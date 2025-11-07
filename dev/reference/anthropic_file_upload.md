# Upload, downloand, and manage files for Claude

**\[experimental\]** Use the beta Files API to upload files to and
manage files in Claude. This is currently experimental because the API
is in beta and may change. Note that you need
`beta-headers = "files-api-2025-04-14"` to use the API.

Claude offers 100GB of file storage per organization, with each file
having a maximum size of 500MB. For more details see
<https://docs.claude.com/en/docs/build-with-claude/files>

- `anthropic_file_upload()` uploads a file and returns an object that
  you can use in chat.

- `anthropic_file_list()` lists all uploaded files.

- `anthropic_file_get()` returns an object for an previously uploaded
  file.

- `anthropic_file_download()` downloads the file with the given ID. Note
  that you can only download files created by skills or the code
  execution tool.

- `anthropic_file_delete()` deletes the file with the given ID.

## Usage

``` r
anthropic_file_upload(
  path,
  base_url = "https://api.anthropic.com/v1/",
  beta_headers = "files-api-2025-04-14",
  credentials = NULL
)

anthropic_file_list(
  base_url = "https://api.anthropic.com/v1/",
  credentials = NULL,
  beta_headers = "files-api-2025-04-14"
)

anthropic_file_get(
  file_id,
  base_url = "https://api.anthropic.com/v1/",
  credentials = NULL,
  beta_headers = "files-api-2025-04-14"
)

anthropic_file_download(
  file_id,
  path,
  base_url = "https://api.anthropic.com/v1/",
  credentials = NULL,
  beta_headers = "files-api-2025-04-14"
)

anthropic_file_delete(
  file_id,
  base_url = "https://api.anthropic.com/v1/",
  credentials = NULL,
  beta_headers = "files-api-2025-04-14"
)
```

## Arguments

- path:

  Path to download the file to.

- base_url:

  The base URL to the endpoint; the default is Claude's public API.

- beta_headers:

  Beta headers to use for the request. Defaults to
  `files-api-2025-04-14`.

- credentials:

  A function that returns the API key to use for authentication.

  You generally should not need this argument; instead set the
  `ANTHROPIC_API_KEY` environment variable. The best place to set this
  is in `.Renviron`, which you can easily edit by calling
  `usethis::edit_r_environ()`.

- file_id:

  ID of the file to get information about, download, or delete.

## Examples

``` r
if (FALSE) { # \dontrun{
file <- anthropic_file_upload("path/to/file.pdf")
chat <- chat_anthropic(beta_headers = "files-api-2025-04-14")
chat$chat("Please summarize the document.", file)
} # }
```
