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

# Copy the tool name and arguments from each server-side tool request onto the
# matching result's @request (paired by id). MCP and code execution results
# arrive with an empty @request, so this populates them for display and for
# shinychat tool cards.
link_server_tool_results <- function(contents) {
  for (pair in server_tool_pairs) {
    requests <- keep(contents, S7_inherits, pair$request)
    contents <- fill_result_requests(contents, requests, pair$result)
  }
  contents
}

# Like link_server_tool_results() but sources requests from the whole
# conversation. Programmatic tool calling pauses a turn after a server-side tool
# request and returns the matching result in a LATER turn (once the local tool
# has run), so the request and result land in different turns. The per-response
# link can't see the earlier request, leaving the result's @request empty. This
# fills any still-empty result by searching `turns` (the full conversation) for
# the request with the same id, so the tool name/arguments are available for
# display (e.g. shinychat tool cards).
relink_server_tool_results <- function(turn, turns) {
  for (pair in server_tool_pairs) {
    needs_link <- some(turn@contents, function(x) {
      S7_inherits(x, pair$result) && !nzchar(x@request@name)
    })
    if (!needs_link) {
      next
    }

    requests <- unlist(
      map(turns, \(t) keep(t@contents, S7_inherits, pair$request)),
      recursive = FALSE
    )
    turn@contents <- fill_result_requests(turn@contents, requests, pair$result)
  }
  turn
}

# Copy the tool name and arguments from `requests` onto the matching results
# (paired by id) that don't carry them yet.
fill_result_requests <- function(contents, requests, result_class) {
  if (length(requests) == 0) {
    return(contents)
  }
  request_map <- set_names(requests, map_chr(requests, \(r) r@id))
  map(contents, function(content) {
    if (!S7_inherits(content, result_class) || nzchar(content@request@name)) {
      return(content)
    }
    req_id <- content@request@id
    if (has_name(request_map, req_id)) {
      matched <- request_map[[req_id]]
      content@request@name <- matched@name
      content@request@arguments <- matched@arguments
    }
    content
  })
}

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

# These inherit from ContentToolRequest/ContentToolResult so that shinychat
# renders them as tool cards (like MCP content). They are server-side tools, so
# is_tool_request()/is_tool_result() exclude them from local execution.

ContentToolRequestCode <- new_class(
  "ContentToolRequestCode",
  parent = ContentToolRequest,
  properties = list(
    json = class_list
  )
)
method(format, ContentToolRequestCode) <- function(x, ...) {
  input <- x@json$input %||% list()
  if (x@name == "bash_code_execution") {
    cli::format_inline(
      "[{.strong bash code execution}]: {.code {input$command}}"
    )
  } else if (x@name == "text_editor_code_execution") {
    cli::format_inline("[{.strong file {input$command}}]: {.file {input$path}}")
  } else {
    paste0(
      cli::format_inline("[{.strong code execution}]:\n"),
      input$code %||% ""
    )
  }
}
# The human-readable body of a code execution result block. Bash and Python
# results carry stdout/stderr; a text-editor `view` carries the file content as
# a string. File-operation results (create, str_replace) report the edit
# instead of any stdout, so they are summarised by code_execution_edit_summary()
# to avoid showing an empty result.
code_execution_result_body <- function(content) {
  parts <- c(
    if (is_string(content$stdout) && nzchar(content$stdout)) content$stdout,
    if (is_string(content$stderr) && nzchar(content$stderr)) content$stderr,
    if (is_string(content$content) && nzchar(content$content)) content$content,
    code_execution_edit_summary(content)
  )
  paste(parts, collapse = "\n")
}

# Summarise a text-editor file-operation result. `str_replace` returns its
# change as a unified-diff hunk in `lines`; `create` reports `is_file_update`
# (FALSE for a new file, TRUE when overwriting an existing one).
code_execution_edit_summary <- function(content) {
  if (length(content$lines) > 0) {
    paste(unlist(content$lines), collapse = "\n")
  } else if (!is.null(content$is_file_update)) {
    if (isTRUE(content$is_file_update)) "File updated." else "File created."
  } else {
    NULL
  }
}

ContentToolResponseCode <- new_class(
  "ContentToolResponseCode",
  parent = ContentToolResult,
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
  body <- code_execution_result_body(content)
  paste(c(header, if (nzchar(body)) body), collapse = "\n")
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

# An MCP tool listing has no chat-completions equivalent; drop it rather than
# echoing the raw provider-specific block into the content array. (Registered
# here, not in provider-openai-compatible.R, which is collated before this
# file defines ContentMcpListTools.)
method(as_json, list(ProviderOpenAICompatible, ContentMcpListTools)) <-
  function(provider, x, ...) {
    NULL
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

# Registry ---------------------------------------------------------------------

# The server-side tool request/result content classes, paired. This is the
# single source of truth for code that treats server-executed tools specially:
# is_tool_request()/is_tool_result() exclude them from local execution, and
# link/relink_server_tool_results() use the pairs to copy request metadata
# onto results. Add new server-side tool content types here.
server_tool_pairs <- list(
  list(request = ContentMcpToolRequest, result = ContentMcpToolResult),
  list(request = ContentToolRequestCode, result = ContentToolResponseCode)
)

is_server_tool_request <- function(x) {
  some(server_tool_pairs, \(pair) S7_inherits(x, pair$request))
}

is_server_tool_result <- function(x) {
  some(server_tool_pairs, \(pair) S7_inherits(x, pair$result))
}
