test_that("ToolBuiltIn has description and annotations", {
  tool <- ToolBuiltIn(
    name = "test_tool",
    description = "A test tool.",
    annotations = tool_annotations(title = "Test Tool", read_only_hint = TRUE),
    json = list(type = "test")
  )

  expect_equal(tool@name, "test_tool")
  expect_equal(tool@description, "A test tool.")
  expect_equal(tool@annotations$title, "Test Tool")
  expect_true(tool@annotations$read_only_hint)
  expect_equal(tool@json, list(type = "test"))
})

test_that("ToolBuiltIn defaults for description and annotations", {
  tool <- ToolBuiltIn(name = "minimal", json = list())

  expect_equal(tool@description, "")
  expect_equal(tool@annotations, list())
})

test_that("claude_tool_code_execution() defaults to the latest stable type", {
  tool <- claude_tool_code_execution()
  expect_s7_class(tool, ToolBuiltIn)
  expect_equal(tool@name, "code_execution")
  expect_equal(tool@json$type, "code_execution_20250825")
  expect_equal(tool@json$name, "code_execution")
})

test_that("claude_tool_code_execution() passes through a custom type", {
  expect_equal(
    claude_tool_code_execution("code_execution_20250522")@json$type,
    "code_execution_20250522"
  )
  expect_equal(
    claude_tool_code_execution("code_execution_99999999")@json$type,
    "code_execution_99999999"
  )
})

test_that("claude_tool_code_execution() rejects a non-string type", {
  expect_snapshot(claude_tool_code_execution(123), error = TRUE)
})

test_that("ContentToolRequestCode inherits from ContentToolRequest", {
  req <- ContentToolRequestCode(
    id = "srvtoolu_1",
    name = "bash_code_execution",
    arguments = list(command = "ls"),
    json = list()
  )
  expect_s7_class(req, ContentToolRequestCode)
  expect_s7_class(req, ContentToolRequest)
  expect_equal(req@arguments, list(command = "ls"))
})

test_that("ContentToolResponseCode inherits from ContentToolResult", {
  res <- ContentToolResponseCode(
    value = "ok",
    request = ContentToolRequest(id = "srvtoolu_1", name = "", arguments = list()),
    json = list()
  )
  expect_s7_class(res, ContentToolResponseCode)
  expect_s7_class(res, ContentToolResult)
  expect_equal(res@value, "ok")
})

test_that("is_tool_request()/is_tool_result() exclude code execution by default", {
  req <- ContentToolRequestCode(
    id = "1",
    name = "bash_code_execution",
    arguments = list(),
    json = list()
  )
  res <- ContentToolResponseCode(
    value = "ok",
    request = ContentToolRequest(id = "1", name = "", arguments = list()),
    json = list()
  )
  expect_false(is_tool_request(req))
  expect_true(is_tool_request(req, local_only = FALSE))
  expect_false(is_tool_result(res))
  expect_true(is_tool_result(res, local_only = FALSE))
})

test_that("is_server_tool_content() recognizes code execution content", {
  req <- ContentToolRequestCode(
    id = "1",
    name = "bash_code_execution",
    arguments = list(),
    json = list()
  )
  res <- ContentToolResponseCode(
    value = "ok",
    request = ContentToolRequest(id = "1", name = "", arguments = list()),
    json = list()
  )
  expect_true(is_server_tool_content(req))
  expect_true(is_server_tool_content(res))
  expect_false(is_server_tool_content(ContentText("hi")))
})

test_that("echo_server_tool_contents() echoes code execution in output mode", {
  turn <- AssistantTurn(list(
    ContentToolRequestCode(
      id = "1",
      name = "bash_code_execution",
      arguments = list(command = "echo hi"),
      json = list(input = list(command = "echo hi"))
    ),
    ContentToolResponseCode(
      value = "hi",
      request = ContentToolRequest(id = "1", name = "", arguments = list()),
      json = list(
        content = list(type = "bash_code_execution_result", stdout = "hi")
      )
    )
  ))

  out <- capture.output(
    echo_server_tool_contents(turn, echo = "output"),
    type = "message"
  )
  out <- paste(out, collapse = "\n")
  expect_match(out, "echo hi")
  expect_match(out, "code execution result")
})

test_that("built-in tools", {
  get_built_in_tools <- function() {
    exports <- getNamespaceExports("ellmer")
    tool_fns <- exports[grepl("_tool_", exports)]

    tools <- list()
    for (fn_name in tool_fns) {
      fn <- getExportedValue("ellmer", fn_name)
      result <- tryCatch(fn(), error = function(e) NULL)
      if (!is.null(result) && S7_inherits(result, ToolBuiltIn)) {
        tools[[fn_name]] <- result
      }
    }
    tools
  }

  expect_gte(length(get_built_in_tools()), 1)

  built_in_tools <- get_built_in_tools()

  for (fn_name in names(built_in_tools)) {
    test_that(paste0(fn_name, "() sets description and annotations"), {
      tool <- built_in_tools[[fn_name]]
      expect_match(tool@description, ".")
      expect_match(tool@annotations$title, ".")
    })
  }
})
