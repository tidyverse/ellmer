test_that("mcp_connector() creates McpConnector object", {
  conn <- mcp_connector("https://example.com/mcp", name = "test")
  expect_s3_class(conn, "ellmer::McpConnector")
  expect_equal(conn@url, "https://example.com/mcp")
  expect_equal(conn@name, "test")
  expect_null(conn@credentials)
  expect_equal(conn@extra, list())
})

test_that("mcp_connector() validates inputs", {
  expect_error(mcp_connector(1, name = "test"))
  expect_error(mcp_connector("url", name = 1))
})

test_that("mcp_connector() accepts credentials function", {
  creds <- function() "my-token"
  conn <- mcp_connector(
    "https://example.com/mcp",
    name = "test",
    credentials = creds
  )
  expect_equal(conn@credentials(), "my-token")
})

test_that("mcp_connector() captures extra args", {
  conn <- mcp_connector(
    "https://example.com/mcp",
    name = "test",
    require_approval = "never"
  )
  expect_equal(conn@extra, list(require_approval = "never"))
})

test_that("register_tool() rejects McpConnector for unsupported providers", {
  chat <- Chat$new(test_provider())
  expect_snapshot(error = TRUE, {
    chat$register_tool(
      mcp_connector("https://example.com/mcp", name = "test")
    )
  })
})
