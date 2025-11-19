# Content types received from and sent to a chatbot

Use these functions if you're writing a package that extends ellmer and
need to customise methods for various types of content. For normal use,
see
[`content_image_url()`](https://ellmer.tidyverse.org/dev/reference/content_image_url.md)
and friends.

ellmer abstracts away differences in the way that different
[Provider](https://ellmer.tidyverse.org/dev/reference/Provider.md)s
represent various types of content, allowing you to more easily write
code that works with any chatbot. This set of classes represents types
of content that can be either sent to and received from a provider:

- `ContentText`: simple text (often in markdown format). This is the
  only type of content that can be streamed live as it's received.

- `ContentImageRemote` and `ContentImageInline`: images, either as a
  pointer to a remote URL or included inline in the object. See
  [`content_image_file()`](https://ellmer.tidyverse.org/dev/reference/content_image_url.md)
  and friends for convenient ways to construct these objects.

- `ContentToolRequest`: a request to perform a tool call (sent by the
  assistant).

- `ContentToolResult`: the result of calling the tool (sent by the
  user). This object is automatically created from the value returned by
  calling the
  [`tool()`](https://ellmer.tidyverse.org/dev/reference/tool.md)
  function. Alternatively, expert users can return a `ContentToolResult`
  from a [`tool()`](https://ellmer.tidyverse.org/dev/reference/tool.md)
  function to include additional data or to customize the display of the
  result.

## Usage

``` r
Content()

ContentText(text = stop("Required"))

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

ContentThinking(thinking = stop("Required"), extra = list())

ContentPDF(
  type = stop("Required"),
  data = stop("Required"),
  filename = stop("Required")
)
```

## Arguments

- text:

  A single string.

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

- extra:

  Additional data.

- value:

  The results of calling the tool function, if it succeeded.

- error:

  The error message, as a string, or the error condition thrown as a
  result of a failure when calling the tool function. Must be `NULL`
  when the tool call is successful.

- request:

  The ContentToolRequest associated with the tool result, automatically
  added by ellmer when evaluating the tool call.

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
