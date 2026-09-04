# Content types received from and sent to a chatbot

Use these functions if you're writing a package that extends ellmer and
need to customise methods for various types of content. For normal use,
see
[`content_image_url()`](https://ellmer.tidyverse.org/reference/content_image_url.md)
and friends.

ellmer abstracts away differences in the way that different
[Provider](https://ellmer.tidyverse.org/reference/Provider.md)s
represent various types of content, allowing you to more easily write
code that works with any chatbot. This set of classes represents types
of content that can be either sent to and received from a provider:

- `ContentText`: simple text (often in markdown format). Text streams
  yield only text and thinking content, while content streams can also
  yield annotations such as citations and web activity.

- `ContentCitation`: provider-supplied evidence metadata associated with
  generated text. Citations are preserved in conversation history but
  are not sent back to providers.

- `ContentImageRemote` and `ContentImageInline`: images, either as a
  pointer to a remote URL or included inline in the object. See
  [`content_image_file()`](https://ellmer.tidyverse.org/reference/content_image_url.md)
  and friends for convenient ways to construct these objects.

- `ContentToolRequest`: a request to perform a tool call (sent by the
  assistant).

- `ContentToolResult`: the result of calling the tool (sent by the
  user). This object is automatically created from the value returned by
  calling the [`tool()`](https://ellmer.tidyverse.org/reference/tool.md)
  function. Alternatively, expert users can return a `ContentToolResult`
  from a [`tool()`](https://ellmer.tidyverse.org/reference/tool.md)
  function to include additional data or to customize the display of the
  result.

## Usage

``` r
Content()

ContentText(text = stop("Required"))

ContentCitation(
  source = NULL,
  grounded_span = NULL,
  cited_quote = NULL,
  extra = NULL
)

ContentImage()

ContentImageRemote(url = stop("Required"), detail = "")

ContentImageInline(type = stop("Required"), data = NULL)

ContentToolRequest(
  id = stop("Required"),
  name = stop("Required"),
  arguments = list(),
  tool = NULL,
  extra = list()
)

ContentToolResult(value = NULL, error = NULL, extra = list(), request = NULL)

ContentUploaded(
  uri = stop("Required"),
  mime_type = "",
  provider = "",
  extra = list()
)

ContentThinking(thinking = stop("Required"), extra = list())

ContentPDF(
  type = stop("Required"),
  data = stop("Required"),
  filename = stop("Required"),
  url = NULL
)

ContentDocument(
  mime_type = stop("Required"),
  data = stop("Required"),
  filename = stop("Required"),
  url = NULL
)
```

## Arguments

- text:

  A single string.

- source:

  A [Source](https://ellmer.tidyverse.org/reference/Source.md)
  identifying the cited evidence, or `NULL` when the provider does not
  supply one.

- grounded_span:

  The answer text grounded by the citation, or `NULL`.

- cited_quote:

  The source-side evidence quoted by the provider, or `NULL`.

- extra:

  Additional data.

- url:

  URL to a remote image.

- detail:

  Not currently used.

- type:

  MIME type of the image.

- data:

  Base64 encoded image data.

- id:

  Tool call id (used to associate a request and a result). Automatically
  managed by ellmer.

- name:

  Function name

- arguments:

  Named list of arguments to call the function with.

- tool:

  ellmer automatically matches a tool request to the tools defined for
  the chatbot. If `NULL`, the request did not match a defined tool.

- value:

  The results of calling the tool function, if it succeeded. `NULL`, a
  string, an atomic vector, a `json`-class object, a Content object, or
  a list of Content objects.

- error:

  The error message, as a string, or the error condition thrown as a
  result of a failure when calling the tool function. Must be `NULL`
  when the tool call is successful.

- request:

  The ContentToolRequest associated with the tool result, automatically
  added by ellmer when evaluating the tool call.

- uri:

  The URI or provider-assigned id of the uploaded file.

- mime_type:

  MIME type of the file or document.

- provider:

  Lowercase name of the provider the file was uploaded to (e.g.
  `"openai"`, `"anthropic"`, `"google"`), or `""` when unknown. Used to
  detect a file uploaded to one provider being used with another.

- thinking:

  The text of the thinking output.

- filename:

  File name, used to identify the PDF.

## Value

S7 objects that all inherit from `Content`

## Examples

``` r
Content()
#> <ellmer::Content>
ContentText("Tell me a joke")
#> <ellmer::ContentText>
#>  @ text: chr "Tell me a joke"
ContentImageRemote("https://www.r-project.org/Rlogo.png")
#> <ellmer::ContentImageRemote>
#>  @ url   : chr "https://www.r-project.org/Rlogo.png"
#>  @ detail: chr ""
ContentToolRequest(id = "abc", name = "mean", arguments = list(x = 1:5))
#> <ellmer::ContentToolRequest>
#>  @ id       : chr "abc"
#>  @ name     : chr "mean"
#>  @ arguments:List of 1
#>  .. $ x: int [1:5] 1 2 3 4 5
#>  @ tool     : NULL
#>  @ extra    : list()
```
