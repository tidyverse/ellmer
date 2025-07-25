#' @include tools-def.R
#' @include utils-S7.R
NULL

#' Format contents into a textual representation
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' These generic functions can be use to convert [Turn] contents or [Content]
#' objects into textual representations.
#'
#' * `contents_text()` is the most minimal and only includes [ContentText]
#'   objects in the output.
#' * `contents_markdown()` returns the text content (which it assumes to be
#'   markdown and does not convert it) plus markdown representations of images
#'   and other content types.
#' * `contents_html()` returns the text content, converted from markdown to
#'   HTML with [commonmark::markdown_html()], plus HTML representations of
#'   images and other content types.
#'
#' These content types will continue to grow and change as ellmer evolves to
#' support more providers and as providers add more content types.
#' @examples
#' turns <- list(
#'   Turn("user", contents = list(
#'     ContentText("What's this image?"),
#'     content_image_url("https://placehold.co/200x200")
#'   )),
#'   Turn("assistant", "It's a placeholder image.")
#' )
#'
#' lapply(turns, contents_text)
#' lapply(turns, contents_markdown)
#' if (rlang::is_installed("commonmark")) {
#'   contents_html(turns[[1]])
#' }
#'
#' @param content The [Turn] or [Content] object to be converted into text.
#'   `contents_markdown()` also accepts [Chat] instances to turn the entire
#'   conversation history into markdown text.
#' @param ... Additional arguments passed to methods.
#'
#' @return A string of text, markdown or HTML.
#' @export
contents_text <- new_generic("contents_text", "content")

#' @rdname contents_text
#' @export
contents_html <- new_generic("contents_html", "content")

#' @rdname contents_text
#' @export
contents_markdown <- new_generic("contents_markdown", "content")


#' Content types received from and sent to a chatbot
#'
#' @description
#' Use these functions if you're writing a package that extends ellmer and need
#' to customise methods for various types of content. For normal use, see
#' [content_image_url()] and friends.
#'
#' ellmer abstracts away differences in the way that different [Provider]s
#' represent various types of content, allowing you to more easily write
#' code that works with any chatbot. This set of classes represents types of
#' content that can be either sent to and received from a provider:
#'
#' * `ContentText`: simple text (often in markdown format). This is the only
#'   type of content that can be streamed live as it's received.
#' * `ContentImageRemote` and `ContentImageInline`: images, either as a pointer
#'   to a remote URL or included inline in the object. See
#'   [content_image_file()] and friends for convenient ways to construct these
#'   objects.
#' * `ContentToolRequest`: a request to perform a tool call (sent by the
#'    assistant).
#' * `ContentToolResult`: the result of calling the tool (sent by the user).
#'   This object is automatically created from the value returned by calling the
#'   [tool()] function. Alternatively, expert users can return a
#'   `ContentToolResult` from a [tool()] function to include additional data or
#'   to customize the display of the result.
#'
#' @export
#' @return S7 objects that all inherit from `Content`
#' @examples
#' Content()
#' ContentText("Tell me a joke")
#' ContentImageRemote("https://www.r-project.org/Rlogo.png")
#' ContentToolRequest(id = "abc", name = "mean", arguments = list(x = 1:5))
Content <- new_class("Content")

method(contents_text, Content) <- function(content) {
  NULL
}

method(contents_markdown, Content) <- function(content) {
  # Fall back to text representation in markdown
  contents_text(content)
}

method(contents_html, Content) <- function(content) {
  NULL
}

#' @rdname Content
#' @export
#' @param text A single string.
ContentText <- new_class(
  "ContentText",
  parent = Content,
  properties = list(text = prop_string()),
)
method(format, ContentText) <- function(x, ...) {
  x@text
}

method(contents_text, ContentText) <- function(content) {
  content@text
}

method(contents_html, ContentText) <- function(content) {
  check_installed("commonmark")
  commonmark::markdown_html(content@text)
}

method(contents_markdown, ContentText) <- function(content) {
  content@text
}

# Images -----------------------------------------------------------------

#' @rdname Content
#' @export
ContentImage <- new_class(
  "ContentImage",
  parent = Content
)

#' @rdname Content
#' @export
#' @param url URL to a remote image.
#' @param detail Not currently used.
ContentImageRemote <- new_class(
  "ContentImageRemote",
  parent = Content,
  properties = list(
    url = prop_string(),
    detail = prop_string(default = "")
  )
)
method(format, ContentImageRemote) <- function(x, ...) {
  cli::format_inline("[{.strong remote image}]: {.url {x@url}}")
}
method(contents_html, ContentImageRemote) <- function(content) {
  sprintf('<img src="%s">', content@url)
}
method(contents_markdown, ContentImageRemote) <- function(content) {
  sprintf('![](%s)', content@url)
}

#' @rdname Content
#' @export
#' @param type MIME type of the image.
#' @param data Base64 encoded image data.
ContentImageInline <- new_class(
  "ContentImageInline",
  parent = Content,
  properties = list(
    type = prop_string(),
    data = prop_string(allow_null = TRUE)
  )
)
method(format, ContentImageInline) <- function(x, ...) {
  cli::format_inline("[{.strong inline image}]")
}
method(contents_html, ContentImageInline) <- function(content) {
  sprintf('<img src="data:%s;base64,%s">', content@type, content@data)
}
method(contents_markdown, ContentImageInline) <- function(content) {
  sprintf('![](data:%s;base64,%s)', content@type, content@data)
}

# Tools ------------------------------------------------------------------

#' @rdname Content
#' @export
#' @param id Tool call id (used to associate a request and a result).
#'   Automatically managed by \pkg{ellmer}.
#' @param name Function name
#' @param arguments Named list of arguments to call the function with.
#' @param tool ellmer automatically matches a tool request to the tools defined
#'   for the chatbot. If `NULL`, the request did not match a defined tool.
ContentToolRequest <- new_class(
  "ContentToolRequest",
  parent = Content,
  properties = list(
    id = prop_string(),
    name = prop_string(),
    arguments = class_list,
    tool = NULL | ToolDef
  )
)
method(format, ContentToolRequest) <- function(
  x,
  ...,
  show = c("all", "call")
) {
  show <- arg_match(show)

  arguments <- tool_request_args(x)
  if (is_tool_result(arguments)) {
    # Failed to convert the arguments so just use unconverted
    arguments <- x@arguments
  }
  call <- call2(x@name, !!!arguments)
  call_str <- deparse(call)
  if (length(call_str) > 1) {
    open <-
      call_str <- paste0(call_str[1], "...)")
  }

  if (show == "call") {
    return(call_str)
  }

  cli::format_inline("[{.strong tool request} ({x@id})]: {call_str}")
}

#' @rdname Content
#' @export
#' @param value The results of calling the tool function, if it succeeded.
#' @param error The error message, as a string, or the error condition thrown
#'   as a result of a failure when calling the tool function. Must be `NULL`
#'   when the tool call is successful.
#' @param extra Optional additional data associated with the tool result that
#'   isn't included in the `value` that's shown to the LLM. Useful for including
#'   additional data for displaying the tool result in a client, like a Shiny
#'   app, without including the data in the response to the LLM.
#' @param request The [ContentToolRequest] associated with the tool result,
#'   automatically added by \pkg{ellmer} when evaluating the tool call.
ContentToolResult <- new_class(
  "ContentToolResult",
  parent = Content,
  properties = list(
    value = class_any,
    error = new_property(
      class = NULL | class_character | new_S3_class("condition"),
      default = NULL,
      validator = function(value) {
        ok <- is.null(value) || is_string(value) || inherits(value, "condition")
        if (ok) {
          return()
        }

        paste0(
          "must be a single string or a condition object, not ",
          obj_type_friendly(value),
          "."
        )
      }
    ),
    extra = class_list,
    request = NULL | ContentToolRequest
  )
)
method(format, ContentToolResult) <- function(
  x,
  ...,
  show = c("all", "header")
) {
  show <- arg_match(show)

  header <- cli::format_inline("[{.strong tool result}  ({x@request@id})]:")

  if (show == "header") {
    return(header)
  }

  if (tool_errored(x)) {
    value <- paste0(cli::col_red("Error: "), tool_error_string(x))
  } else {
    value <- tool_string(x)
  }

  if (!is_string(value) || !grepl("\n", value)) {
    paste0(header, " ", value)
  } else {
    paste(c(header, value), collapse = "\n")
  }
}

tool_errored <- function(x) !is.null(x@error)
tool_error_string <- function(x) {
  if (inherits(x@error, "condition")) conditionMessage(x@error) else x@error
}
tool_string <- function(x) {
  if (tool_errored(x)) {
    paste0("Tool calling failed with error ", tool_error_string(x))
  } else if (inherits(x@value, "AsIs")) {
    x@value
  } else if (inherits(x@value, "json")) {
    x@value
  } else if (is.character(x@value)) {
    paste(x@value, collapse = "\n")
  } else {
    jsonlite::toJSON(x@value, auto_unbox = TRUE)
  }
}

ContentJson <- new_class(
  "ContentJson",
  parent = Content,
  properties = list(value = class_any)
)
method(format, ContentJson) <- function(x, ...) {
  paste0(
    cli::format_inline("[{.strong data}] "),
    pretty_json(x@value)
  )
}
method(contents_html, ContentJson) <- function(content) {
  sprintf('<pre><code>%s</code></pre>\n', pretty_json(content@value))
}
method(contents_markdown, ContentJson) <- function(content) {
  sprintf('```json\n%s\n```\n', pretty_json(content@value))
}

ContentUploaded <- new_class(
  "ContentUploaded",
  parent = Content,
  properties = list(
    uri = prop_string(),
    mime_type = prop_string(default = "")
  )
)
method(format, ContentUploaded) <- function(x, ...) {
  cli::format_inline("[{.strong uploaded file}]: [{x@mime_type}]")
}
method(contents_html, ContentUploaded) <- function(content) {
  sprintf("[uploaded %s file]", content@mime_type)
}
method(contents_markdown, ContentUploaded) <- function(content) {
  sprintf("`[uploaded %s file]`", content@mime_type)
}

# Thinking ---------------------------------------------------------------------

#' @rdname Content
#' @param thinking The text of the thinking output.
#' @param extra Additional data.
#' @export
ContentThinking <- new_class(
  "ContentThinking",
  parent = Content,
  properties = list(
    thinking = prop_string(),
    extra = class_list
  )
)

method(format, ContentThinking) <- function(x, ...) {
  paste0("<thinking>\n", x@thinking, "\n</thinking>\n")
}

method(contents_html, ContentThinking) <- function(content) {
  check_installed("commonmark")
  paste0(
    "<details><summary>Thinking</summary>\n",
    commonmark::markdown_html(content@thinking),
    "</details>\n"
  )
}

method(contents_markdown, ContentThinking) <- function(content) {
  format(content)
}

# Helpers ----------------------------------------------------------------------

as_content <- function(x, error_call = caller_env(), error_arg = "...") {
  if (is.null(x)) {
    list()
  } else if (is_prompt(x)) {
    if (length(x) == 1) {
      ContentText(x[[1]])
    } else {
      cli::cli_abort(
        "{.arg {error_arg}} can only accept a single prompt.",
        call = error_call
      )
    }
  } else if (is.character(x)) {
    ContentText(paste0(x, collapse = "\n\n"))
  } else if (S7_inherits(x, Content)) {
    x
  } else {
    stop_input_type(
      x,
      what = "made up strings or <content> objects",
      arg = error_arg,
      error_call = error_call
    )
  }
}

#' @rdname Content
#' @param filename File name, used to identify the PDF.
#' @export
ContentPDF <- new_class(
  "ContentPDF",
  parent = Content,
  properties = list(
    type = prop_string(),
    data = prop_string(),
    filename = prop_string()
  )
)

method(format, ContentPDF) <- function(x, ...) {
  "<PDF document>"
}
