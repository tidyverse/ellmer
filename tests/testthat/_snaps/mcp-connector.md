# register_tool() rejects McpConnector for unsupported providers

    Code
      chat$register_tool(mcp_connector("https://example.com/mcp", name = "test"))
    Condition
      Error in `chat$register_tool()`:
      ! MCP connectors are not supported by <ellmer::Provider>.

