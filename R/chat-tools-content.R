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
  list(
    ContentToolResult(value = "See <tool-content> below", request = request),
    ContentText(sprintf('<tool-content call-id="%s">', request@id)),
    value,
    ContentText("</tool-content>")
  )
}
expand_tool_values <- function(request, values) {
  result <- ContentToolResult(
    value = "See <tool-contents> below",
    request = request
  )

  contents <- map(values, function(value) {
    list(ContentText("<tool-content>"), value, ContentText("</tool-content>"))
  })

  c(
    list(result, ContentText("<tool-contents/>")),
    unlist(contents, recursive = FALSE),
    list(ContentText("</tool-contents>"))
  )
}
