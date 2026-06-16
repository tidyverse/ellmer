# tool_context() outside a tool gives informative error

    Code
      tool_context()
    Condition
      Error in `tool_context()`:
      ! No tool context is available.
      i `tool_context()` must be called from inside an active tool invocation. If you are writing an async tool, capture the context before any `await()` call: `ctx <- tool_context()`.

# as_tool_context() errors on non-list, non-context input

    Code
      with_tool_context("not a list", NULL)
    Condition
      Error in `push_tool_context()`:
      ! `context` must be an <ellmer_tool_context> object or a list.

