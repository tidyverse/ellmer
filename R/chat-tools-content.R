# Very few providers support anything other than text results; fortunately we
# can fake them them by unrolling the tool result to a forward pointer to
# other user content items.
#
# ContentToolResult(value = ContentImageInline(...))
# ->
# ContentToolResult("See <content-tool> below")
# ContentText("<content-tool>")
# ContentImageInline(...)
# ContentText("</content-tool>")

turn_contents_expand <- function(turn) {
  if (length(turn@contents) == 0) {
    # Early return to avoid unlist(list()) yielding NULL
    return(turn)
  }

  contents <- map(turn@contents, expand_content_if_needed)
  turn@contents <- unlist(contents, recursive = FALSE)
  turn
}

expand_content_if_needed <- function(content) {
  if (!is_tool_result(content)) {
    return(list(content))
  }

  request <- content@request
  value <- content@value

  if (S7_inherits(value, Content)) {
    expand_tool_value(request, value)
  } else if (is.list(value)) {
    if (all(map_lgl(value, \(x) S7_inherits(x, Content)))) {
      expand_tool_values(request, value)
    } else {
      list(content)
    }
  } else {
    list(content)
  }
}

expand_tool_value <- function(request, value) {
  open <- sprintf('<tool-content call-id="%s">', request@id)
  list(
    ContentToolResult(
      value = sprintf("See %s below.", open),
      request = request
    ),
    ContentText(open),
    value,
    ContentText("</tool-content>")
  )
}

expand_tool_values <- function(request, values) {
  open <- sprintf('<tool-contents call-id="%s">', request@id)
  result <- ContentToolResult(
    value = sprintf('See %s below.', open),
    request = request
  )

  contents <- map(values, function(value) {
    list(ContentText("<tool-content>"), value, ContentText("</tool-content>"))
  })
  contents <- unlist(contents, recursive = FALSE)

  open <- ContentText(open)
  close <- ContentText("</tool-contents>")
  c(list(result, open), contents, list(close))
}
