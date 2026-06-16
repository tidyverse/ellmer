# tool_context() outside a tool gives informative error

    Code
      tool_context()
    Condition
      Error in `tool_context()`:
      ! No tool context is available.
      i `tool_context()` must be called from inside an active tool invocation. If you are writing an async tool, capture the context before any `await()` call: `ctx <- tool_context()`.

