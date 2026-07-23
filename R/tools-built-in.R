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

# MCP tool use ----------------------------------------------------------------

ContentToolRequestMcp <- new_class(
  "ContentToolRequestMcp",
  parent = Content,
  properties = list(
    id = prop_string(),
    name = prop_string(),
    server_name = prop_string(),
    json = class_list
  )
)
method(format, ContentToolRequestMcp) <- function(x, ...) {
  cli::format_inline("[{.strong mcp tool request}]: {x@server_name}/{x@name}")
}
method(as_json, list(Provider, ContentToolRequestMcp)) <- function(
  provider,
  x,
  ...
) {
  x@json
}

ContentToolResponseMcp <- new_class(
  "ContentToolResponseMcp",
  parent = Content,
  properties = list(
    tool_use_id = prop_string(),
    json = class_list
  )
)
method(format, ContentToolResponseMcp) <- function(x, ...) {
  cli::format_inline("[{.strong mcp tool result}]: {x@tool_use_id}")
}
method(as_json, list(Provider, ContentToolResponseMcp)) <- function(
  provider,
  x,
  ...
) {
  x@json
}
