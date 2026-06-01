# Design: `claude_tool_code_execution()`

Date: 2026-06-01
Status: Approved (pending spec review)

## Overview

Add a new Claude server-side built-in tool, `claude_tool_code_execution()`, that
lets Claude run Python/Bash code and manipulate files inside Anthropic's
sandboxed container. This mirrors the existing `claude_tool_web_search()` and
`claude_tool_web_fetch()` tools: a `ToolBuiltIn` object carries the tool spec
sent to the API, while `server_tool_use` request blocks and
`*_code_execution_tool_result` response blocks are parsed into `Content`
subclasses for echo display and conversation round-tripping.

The sandbox is fully isolated (no internet, 5 GiB RAM/disk, 1 CPU, Python
3.11) and runs entirely on Anthropic's servers — ellmer sends the tool spec and
renders the requests/results; it does not execute any code locally.

## Goals

- Register the code execution tool via `chat$register_tool(claude_tool_code_execution())`.
- Support all three tool versions, defaulting to the broadly compatible one.
- Auto-inject the matching `code-execution-*` beta header so users don't have
  to remember it.
- Parse and nicely display code execution requests and results (stdout, stderr,
  return code, bash, file operations, errors).

## Non-goals (out of scope for this version)

These remain achievable today through `api_args` / `extra_args` and are not
given first-class helpers:

- Container reuse across requests (`api_args = list(container = "<id>")`).
- Files API integration: `container_upload` content blocks and retrieval of
  files Claude generates in the sandbox.
- `pause_turn` auto-continuation. ellmer captures `stop_reason` but does not
  auto-continue the turn for *any* server-side tool (web search/fetch included).
  This is a cross-cutting concern, documented here as a known limitation.
- Cost tracking of `usage.server_tool_use.code_execution_requests` (code
  execution is billed by container-time, which is not derivable from the
  response body).

## Public API

In `R/provider-claude-tools.R`:

```r
claude_tool_code_execution(version = c("20250825", "20250522", "20260120"))
```

- `version` is validated with `arg_match()` and maps to the tool `type`:

  | `version`    | tool `type`               | capabilities                                              | models                       |
  |--------------|---------------------------|-----------------------------------------------------------|------------------------------|
  | `"20250825"` | `code_execution_20250825` | Python + Bash + file operations                           | all current models (default) |
  | `"20250522"` | `code_execution_20250522` | Python only (legacy)                                      | all                          |
  | `"20260120"` | `code_execution_20260120` | adds REPL state persistence + programmatic tool calling   | Opus 4.5+ / Sonnet 4.5+ only |

- The tool's API `name` is always `"code_execution"` for every version; only
  `type` differs.
- Returns:

  ```r
  ToolBuiltIn(
    name = "code_execution",
    description = "Run Python and bash code in a sandboxed container.",
    annotations = tool_annotations(
      title = "Code execution",
      read_only_hint = FALSE,
      open_world_hint = FALSE
    ),
    json = list(type = type, name = "code_execution")
  )
  ```

  `read_only_hint = FALSE` (it executes code / writes files); `open_world_hint
  = FALSE` (the sandbox has no internet access).

A single source-of-truth lookup lives next to the function and is shared with
the beta-header logic:

```r
code_execution_betas_lookup <- c(
  code_execution_20250522 = "code-execution-2025-05-22",
  code_execution_20250825 = "code-execution-2025-08-25",
  code_execution_20260120 = "code-execution-2026-01-20"
)
```

## Beta header auto-injection

A helper scans registered tools for code-execution `ToolBuiltIn`s and returns
the beta headers they require:

```r
code_execution_betas <- function(tools) {
  types <- map_chr(tools, function(t) {
    if (S7_inherits(t, ToolBuiltIn)) t@json$type %||% "" else ""
  })
  betas <- code_execution_betas_lookup[types]
  unique(unname(betas[!is.na(betas)]))
}
```

`chat_request()` in `R/provider-claude.R` (currently around line 788) is
restructured so that all auto-injected betas are merged in one place instead of
the present MCP-only branch:

```r
auto_betas <- character()
if (has_mcp) auto_betas <- c(auto_betas, "mcp-client-2025-11-20")
auto_betas <- c(auto_betas, code_execution_betas(tools))
if (length(auto_betas) > 0) {
  beta <- unique(c(provider@beta_headers, auto_betas))
  req <- req_headers(req, `anthropic-beta` = paste(beta, collapse = ","))
}
```

This preserves any user-supplied `beta_headers` and the existing MCP behavior.

## Content classes

Two new classes in `R/tools-built-in.R`, following the `ContentToolRequestSearch`
/ `ContentToolResponseSearch` precedent: both extend `Content`, carry the raw
`json`, and define `as_json()` returning `x@json` verbatim for round-tripping.
Unlike Search/Fetch (which have a single request and result shape each), code
execution has three sub-tools and three result block types under one tool, so
each class branches internally in `format()`.

### `ContentToolRequestCode`

- Properties: `name` (the `server_tool_use` name), `json`.
- `format()` branches on `name`:
  - `code_execution` (legacy Python) → show `input$code`
  - `bash_code_execution` → show `input$command`
  - `text_editor_code_execution` → show `input$command` + `input$path`
- `as_json()` → `x@json`.

### `ContentToolResponseCode`

- Properties: `json`.
- `format()` branches on the result content `type`:
  - `*_result` blocks → `return_code` plus `stdout` / `stderr` (file-operation
    results show a success / diff summary)
  - `*_error` blocks → `error_code`
- `as_json()` → `x@json`.

## Parsing

In `R/provider-claude.R` content parsing (currently lines ~443-469):

- Extend the `server_tool_use` branch: when
  `content$name %in% c("code_execution", "bash_code_execution", "text_editor_code_execution")`,
  build `ContentToolRequestCode(name = content$name, json = content)` (after the
  existing `is_string(content$input)` JSON-parse step).
- Add a result branch: when `content$type %in% c("code_execution_tool_result",
  "bash_code_execution_tool_result", "text_editor_code_execution_tool_result")`,
  build `ContentToolResponseCode(json = content)`.

The existing `web_search` / `web_fetch` branches and the unknown-server-tool
`cli_abort` are preserved.

## Reference: API block shapes

Request (`server_tool_use`):

```json
{ "type": "server_tool_use", "id": "srvtoolu_…", "name": "bash_code_execution",
  "input": { "command": "ls -la | head -5" } }
```

Bash result:

```json
{ "type": "bash_code_execution_tool_result", "tool_use_id": "srvtoolu_…",
  "content": { "type": "bash_code_execution_result",
    "stdout": "…", "stderr": "", "return_code": 0 } }
```

Error result:

```json
{ "type": "bash_code_execution_tool_result", "tool_use_id": "srvtoolu_…",
  "content": { "type": "bash_code_execution_tool_result_error",
    "error_code": "unavailable" } }
```

Error codes: `unavailable`, `execution_time_exceeded`, `container_expired`,
`invalid_tool_input`, `too_many_requests`, `output_file_too_large` (bash),
`file_not_found` / `string_not_found` (text editor).

## Testing

No live network is required; the existing tests parse synthetic response lists
through `value_turn(provider, result)`.

`tests/testthat/test-tools-built-in.R`:

- `claude_tool_code_execution()` sets the correct `type` for each `version`.
- An invalid `version` produces a snapshot error (`arg_match`).
- The existing "all built-in tools" loop automatically covers
  description/annotations.

`tests/testthat/test-provider-claude.R` (synthetic `value_turn()` tests):

- A `bash_code_execution` request parses to `ContentToolRequestCode` with the
  right `name`.
- A `text_editor_code_execution` request parses to `ContentToolRequestCode`.
- A `bash_code_execution_tool_result` parses to `ContentToolResponseCode`;
  `format()` shows stdout / return code.
- A `*_tool_result_error` block parses to `ContentToolResponseCode`; `format()`
  shows the error code.
- `chat_request()` includes `code-execution-2025-08-25` in the `anthropic-beta`
  header when the tool is registered, and that it merges with both MCP and
  user-supplied betas.

No VCR cassette is recorded in this version (recording needs network + an API
key). An optional integration cassette (`_vcr/anthropic-code-execution.yml`)
can be recorded later by someone with API access, matching
`_vcr/anthropic-web-search.yml`.

## Documentation

- Roxygen for `claude_tool_code_execution()` with `@family built-in tools`, a
  `@param version` description, and a `\dontrun{}` example. Comments wrapped at
  80 characters.
- Add the topic to `_pkgdown.yml` (alongside the other built-in tools).
- Add a `NEWS.md` bullet led by the function name.
- Run `devtools::document()` and `pkgdown::check_pkgdown()`.

## Files touched

- `R/provider-claude-tools.R` — `claude_tool_code_execution()`, lookup table,
  `code_execution_betas()` helper.
- `R/tools-built-in.R` — `ContentToolRequestCode`, `ContentToolResponseCode`
  (+ `format` / `as_json` methods).
- `R/provider-claude.R` — request/result parsing branches; `chat_request()`
  beta-header merge.
- `tests/testthat/test-tools-built-in.R`, `tests/testthat/test-provider-claude.R`.
- `man/claude_tool_code_execution.Rd` (generated), `NAMESPACE` (generated),
  `_pkgdown.yml`, `NEWS.md`.
