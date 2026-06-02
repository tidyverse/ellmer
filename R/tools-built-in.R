#' @include tools-def.R
#' @include as-json.R

ToolBuiltIn <- new_class(
  "ToolBuiltIn",
  properties = list(
    name = prop_string(),
    description = prop_string(default = ""),
    annotations = class_list,
    json = class_any
  )
)

method(as_json, list(Provider, ToolBuiltIn)) <- function(provider, x, ...) {
  x@json
}

# Web search ---------------------------------------------------------------

ContentToolRequestSearch <- new_class(
  "ContentToolRequestSearch",
  parent = Content,
  properties = list(
    query = prop_string(),
    json = class_list
  )
)
method(format, ContentToolRequestSearch) <- function(x, ...) {
  cli::format_inline("[{.strong web search request}]: {.str {x@query}}")
}
method(as_json, list(Provider, ContentToolRequestSearch)) <- function(
  provider,
  x,
  ...
) {
  x@json
}

ContentToolResponseSearch <- new_class(
  "ContentToolResponseSearch",
  parent = Content,
  properties = list(
    urls = class_character,
    json = class_list
  )
)
method(format, ContentToolResponseSearch) <- function(x, ...) {
  paste0(
    cli::format_inline("[{.strong web search results}]:\n"),
    paste0("* ", x@urls, "\n", collapse = "")
  )
}

method(as_json, list(Provider, ContentToolResponseSearch)) <- function(
  provider,
  x,
  ...
) {
  x@json
}

# Web fetch -------------------------------------------------------------------

ContentToolRequestFetch <- new_class(
  "ContentToolRequestFetch",
  parent = Content,
  properties = list(
    url = prop_string(),
    json = class_list
  )
)
method(format, ContentToolRequestFetch) <- function(x, ...) {
  cli::format_inline("[{.strong web fetch request}]: {.url {x@url}}")
}
method(as_json, list(Provider, ContentToolRequestFetch)) <- function(
  provider,
  x,
  ...
) {
  x@json
}

ContentToolResponseFetch <- new_class(
  "ContentToolResponseFetch",
  parent = Content,
  properties = list(
    url = prop_string(),
    json = class_list
  )
)
method(format, ContentToolResponseFetch) <- function(x, ...) {
  cli::format_inline("[{.strong web fetch result}]: {.url {x@url}}")
}
method(as_json, list(Provider, ContentToolResponseFetch)) <- function(
  provider,
  x,
  ...
) {
  x@json
}

# Code execution --------------------------------------------------------------

ContentToolRequestCode <- new_class(
  "ContentToolRequestCode",
  parent = Content,
  properties = list(
    name = prop_string(),
    json = class_list
  )
)
method(format, ContentToolRequestCode) <- function(x, ...) {
  input <- x@json$input %||% list()
  if (x@name == "bash_code_execution") {
    cli::format_inline("[{.strong bash code execution}]: {.code {input$command}}")
  } else if (x@name == "text_editor_code_execution") {
    cli::format_inline("[{.strong file {input$command}}]: {.file {input$path}}")
  } else {
    paste0(
      cli::format_inline("[{.strong code execution}]:\n"),
      input$code %||% ""
    )
  }
}
method(as_json, list(Provider, ContentToolRequestCode)) <- function(
  provider,
  x,
  ...
) {
  x@json
}

ContentToolResponseCode <- new_class(
  "ContentToolResponseCode",
  parent = Content,
  properties = list(
    json = class_list
  )
)
method(format, ContentToolResponseCode) <- function(x, ...) {
  content <- x@json$content %||% list()
  type <- content$type %||% ""

  if (grepl("_error$", type)) {
    return(cli::format_inline(
      "[{.strong code execution error}]: {.str {content$error_code %||% 'unknown'}}"
    ))
  }

  header <- if (!is.null(content$return_code)) {
    cli::format_inline(
      "[{.strong code execution result}] (exit {content$return_code})"
    )
  } else {
    cli::format_inline("[{.strong code execution result}]")
  }
  body <- c(
    if (is_string(content$stdout) && nzchar(content$stdout)) content$stdout,
    if (is_string(content$stderr) && nzchar(content$stderr)) content$stderr,
    if (is_string(content$content) && nzchar(content$content)) content$content
  )
  paste(c(header, body), collapse = "\n")
}
method(as_json, list(Provider, ContentToolResponseCode)) <- function(
  provider,
  x,
  ...
) {
  x@json
}

# MCP tool use ----------------------------------------------------------------

mcp_tool_def <- function(name, server_name) {
  ToolDef(
    function() invisible(),
    name = paste0(server_name, "__", name),
    description = "",
    arguments = TypeObject(properties = list()),
    convert = FALSE
  )
}

ContentMcpListTools <- new_class(
  "ContentMcpListTools",
  parent = Content,
  properties = list(
    server_name = prop_string(),
    tools = class_list,
    json = class_list
  )
)
method(format, ContentMcpListTools) <- function(x, ...) {
  n <- length(x@tools)
  tool_names <- vapply(x@tools, function(t) t$name %||% "?", character(1))
  label <- paste0(tool_names, collapse = ", ")
  cli::format_inline(
    "[{.strong mcp tools}] ({x@server_name}): {n} tool{?s} ({label})"
  )
}
method(as_json, list(Provider, ContentMcpListTools)) <- function(
  provider,
  x,
  ...
) {
  x@json
}

ContentMcpToolRequest <- new_class(
  "ContentMcpToolRequest",
  parent = ContentToolRequest,
  properties = list(
    server_name = prop_string(),
    json = class_list
  )
)
method(format, ContentMcpToolRequest) <- function(
  x,
  ...,
  show = c("all", "call", "call_short"),
  label = "mcp tool request"
) {
  tmp <- ContentToolRequest(
    id = x@id,
    name = paste0(x@server_name, "__", x@name),
    arguments = x@arguments
  )
  format(tmp, ..., show = show, label = label)
}
method(as_json, list(Provider, ContentMcpToolRequest)) <- function(
  provider,
  x,
  ...
) {
  x@json
}

ContentMcpToolResult <- new_class(
  "ContentMcpToolResult",
  parent = ContentToolResult,
  properties = list(
    content = class_list,
    json = class_list
  )
)
method(format, ContentMcpToolResult) <- function(
  x,
  ...,
  show = c("all", "header", "value"),
  label = "mcp tool result"
) {
  tmp <- ContentToolResult(
    value = x@value,
    error = x@error,
    request = x@request
  )
  format(tmp, ..., show = show, label = label)
}
method(as_json, list(Provider, ContentMcpToolResult)) <- function(
  provider,
  x,
  ...
) {
  x@json
}
