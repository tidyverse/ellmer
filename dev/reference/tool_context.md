# Access the current tool context

When an ellmer tool is called by an LLM, `tool_context()` returns a
context object with two fields:

- `$request`: the
  [ContentToolRequest](https://ellmer.tidyverse.org/dev/reference/Content.md)
  for this call, with sub-fields `@name`, `@id`, `@arguments`, and
  `@tool`.

- `$turns`: an eager snapshot of the conversation history (list of
  [Turn](https://ellmer.tidyverse.org/dev/reference/Turn.md) objects),
  including the system prompt (if any) as the first turn, up to and
  including the assistant turn that issued this tool request. Sibling
  tool results from the same turn are not included (they are appended
  after the current tool loop finishes).

`tool_context()` aborts with class
`ellmer_error_tool_context_unavailable` if the stack is empty, which
happens in two situations:

1.  The function was called outside any tool invocation (e.g. in a test
    or top-level script without a live chat).

2.  The function was called after an `await()` in an async tool. The
    context frame closes at the first `await`, so you must capture the
    context before yielding: `ctx <- tool_context()`.

The `with_tool_context()` and `local_tool_context()` helpers are useful
for testing a tool function that calls `tool_context()` outside a live
chat. They temporarily make a supplied context available while test code
runs.

## Usage

``` r
tool_context()

with_tool_context(context, code)

local_tool_context(context, .frame = parent.frame())
```

## Arguments

- context:

  An `ellmer_tool_context` object, or a list with fields `request` and
  `turns` (which will be promoted automatically).

- code:

  An expression to evaluate with `context` on top of the stack.

- .frame:

  The environment whose exit triggers the pop. Defaults to
  [`parent.frame()`](https://rdrr.io/r/base/sys.parent.html) (the
  calling function's frame).

## Value

`tool_context()` returns the current `ellmer_tool_context` object (a
classed list with fields `$request`, `$turns`).

`with_tool_context()` returns the value of `code`.

`local_tool_context()` returns `context` invisibly.

## Examples

``` r
# Log the id of the request that triggered this tool call
logging_tool <- tool(
  function() {
    ctx <- tool_context()
    message("Handled request ", ctx$request@id)
    "done"
  },
  name = "logging_tool",
  description = "Log the current request id and return"
)

# Test a tool that uses tool_context() without a live chat
request <- ContentToolRequest(
  id = "test-request",
  name = "logging_tool",
  arguments = list(),
  tool = logging_tool
)
with_tool_context(
  list(request = request, turns = list()),
  logging_tool()
)
#> Handled request test-request
#> [1] "done"
```
