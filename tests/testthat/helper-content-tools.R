fixture_list_of_tools <- function() {
  list(
    tool_scalar = tool(function() 1, "Tool", .name = "tool_scalar"),
    my_tool = tool(function() 1, "Tool", .name = "my_tool"),
    tool_list = tool(
      function() list(a = 1, b = 2),
      "Tool",
      .name = "tool_list"
    ),
    tool_chr = tool(function() letters[1:3], "Tool", .name = "tool_chr"),
    tool_abort = tool(.description = "Tool", .name = "tool_abort", function() {
      cli::cli_abort(c(
        "Unexpected input",
        "i" = "Please revise and try again."
      ))
    })
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

  Turn(
    "assistant",
    list(req_success, req_fail, req_list, req_chr, req_abort)
  )
}
