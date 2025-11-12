fixture_list_of_tools <- function() {
  list(
    tool_scalar = tool(
      function() 1,
      name = "tool_scalar",
      description = "Tool"
    ),
    my_tool = tool(
      function() 1,
      name = "my_tool",
      description = "Tool"
    ),
    tool_list = tool(
      function() list(a = 1, b = 2),
      name = "tool_list",
      description = "Tool"
    ),
    tool_chr = tool(
      function() letters[1:3],
      name = "tool_chr",
      description = "Tool"
    ),
    tool_abort = tool(
      function() {
        cli::cli_abort(c(
          "Unexpected input",
          "i" = "Please revise and try again."
        ))
      },
      name = "tool_abort",
      description = "Tool"
    )
  )
}

fixture_turn_with_tool_requests <- function(with_tool = TRUE) {
  tools <- fixture_list_of_tools()

  req_success <- ContentToolRequest(
    id = "x1",
    name = "my_tool",
    arguments = list(),
    tool = if (with_tool) tools$my_tool
  )
  req_fail <- ContentToolRequest(
    id = "x2",
    name = "my_tool",
    arguments = list(x = 1),
    tool = if (with_tool) tools$my_tool
  )
  req_list <- ContentToolRequest(
    id = "x3",
    name = "tool_list",
    arguments = list(),
    tool = if (with_tool) tools$tool_list
  )
  req_chr <- ContentToolRequest(
    id = "x4",
    name = "tool_chr",
    arguments = list(),
    tool = if (with_tool) tools$tool_chr
  )
  req_abort <- ContentToolRequest(
    id = "x5",
    name = "tool_abort",
    arguments = list(),
    tool = if (with_tool) tools$tool_abort
  )

  AssistantTurn(
    list(req_success, req_fail, req_list, req_chr, req_abort)
  )
}
