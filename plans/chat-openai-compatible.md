# Plan: Rename chat_openai to chat_openai_compatible

## Overview
Rename `chat_openai()` → `chat_openai_compatible()` and `chat_openai_responses()` → `chat_openai()` to better reflect their purposes. The compatible version is for OpenAI-compatible APIs (not official OpenAI), while the main version uses OpenAI's responses API.

## Design Decisions
1. **Hard rename** - No deprecation period, breaking change
2. **Remove default base_url** - `chat_openai_compatible()` will require explicit `base_url` parameter
3. **Keep credentials-only API** - New `chat_openai()` won't add `api_key` parameter
4. **Rename test helpers** - Update `_test()` functions to match new names

## Implementation Steps

### 1. Core Function Refactoring

#### R/provider-openai.R → R/provider-openai-compatible.R
- **Rename file**: `provider-openai.R` → `provider-openai-compatible.R`
- Rename `chat_openai()` → `chat_openai_compatible()`
- Rename `chat_openai_test()` → `chat_openai_compatible_test()`
- Rename class: `ProviderOpenAI` → `ProviderOpenAICompatible`
- Remove default value from `base_url` parameter (make it required)
- Update documentation:
  - Title: "Chat with OpenAI-compatible API"
  - Description: Emphasize this is for OpenAI-compatible APIs (Ollama, vLLM, etc.), NOT for official OpenAI
  - Add note directing users to `chat_openai()` for official OpenAI API
  - Update `base_url` parameter docs to indicate it's required
- Update `@family chatbots` tag references

#### R/provider-openai-responses.R → R/provider-openai.R
- **Rename file**: `provider-openai-responses.R` → `provider-openai.R`
- Rename `chat_openai_responses()` → `chat_openai()`
- Rename `chat_openai_responses_test()` → `chat_openai_test()`
- Rename class: `ProviderOpenAIResponses` → `ProviderOpenAI`
- Update documentation:
  - Title: "Chat with OpenAI"
  - Description: Main function for official OpenAI API using responses endpoint
  - Keep all parameters as-is (no `api_key` parameter)

#### R/provider-openai-models.R (if exists)
- Update `models_openai()` documentation if it references the renamed functions

### 2. Provider Files Using @inheritParams

**19 provider files** use `@inheritParams chat_openai` - update to `@inheritParams chat_openai_compatible`:
- R/provider-vllm.R - Update wrapper description
- R/provider-snowflake.R
- R/provider-portkey.R
- R/provider-perplexity.R - Update wrapper description
- R/provider-openrouter.R
- R/provider-ollama.R - Update wrapper description
- R/provider-mistral.R
- R/provider-huggingface.R - Update wrapper description
- R/provider-groq.R - Update wrapper description
- R/provider-github.R
- R/provider-deepseek.R
- R/provider-databricks.R
- R/provider-cloudflare.R
- R/provider-azure.R
- R/provider-aws.R
- R/provider-any.R

**Note:** Some providers (vLLM, Perplexity, Ollama, Huggingface, Groq) describe themselves as "lightweight wrapper around chat_openai()" - update to say "Uses OpenAI compatible API via `chat_openai_compatible()`"

### 3. Provider Files Using @inherit return

**17 provider files** use `@inherit chat_openai return` - keep as is since return type is the same.

### 4. Test Files

Update all test files using the renamed functions:

#### tests/testthat/test-provider-openai.R → tests/testthat/test-provider-openai-compatible.R
- **Rename file**: `test-provider-openai.R` → `test-provider-openai-compatible.R`
- Rename all `chat_openai_test()` → `chat_openai_compatible_test()` (11 instances)
- Update test descriptions/context as needed

#### tests/testthat/test-provider-openai-responses.R → tests/testthat/test-provider-openai.R
- **Rename file**: `test-provider-openai-responses.R` → `test-provider-openai.R`
- Rename all `chat_openai_responses_test()` → `chat_openai_test()` (10 instances)
- Update test descriptions/context as needed

#### Other test files using chat_openai_test()
- tests/testthat/test-parallel-chat.R
- tests/testthat/test-batch-chat.R
- tests/testthat/test-content.R
- tests/testthat/test-chat.R
- tests/testthat/test-chat-tools.R
- tests/testthat/test-chat-structured.R
- tests/testthat/test-utils-auth.R
- tests/testthat/test-chat-utils.R

**Decision:** These tests should continue to use `chat_openai_test()` so they use the new responses API.

#### Snapshot files
These will auto-update when tests run:
- tests/testthat/_snaps/provider-openai.md → tests/testthat/_snaps/provider-openai-compatible.md
- tests/testthat/_snaps/provider-openai-responses.md → tests/testthat/_snaps/provider-openai.md
- tests/testthat/_snaps/utils-auth.md
- tests/testthat/_snaps/chat.md
- tests/testthat/_snaps/content-replay.md
- tests/testthat/_snaps/batch-chat.md

### 5. Vignettes

Should not need changes; continue to use `chat_openai()` in order to use latest API.

### 6. README Files

#### README.Rmd (4 usages)
- Update to `chat_openai()` (responses API)
- Use `devtools::build_readme()` to update rendered .md

### 7. Source Code Documentation

Update function documentation and examples:

#### R/live.R
- Update `@param chat` documentation mentioning `chat_openai()`

#### R/content-image.R
- Update examples using `chat_openai()`

#### R/batch-chat.R
- Update documentation mentioning `chat_openai()`

#### R/parallel-chat.R
- Update documentation mentioning `chat_openai()`

#### R/tools-def-auto.R
- Update `create_tool_def()` defaults using `chat_openai()`

### 8. Man Pages

These will be regenerated by `devtools::document()`

### 9. NAMESPACE

Update exports:
- Remove: `export(chat_openai)` (OLD meaning)
- Remove: `export(chat_openai_responses)`
- Add: `export(chat_openai)` (NEW meaning - responses API)
- Add: `export(chat_openai_compatible)`

Note: `devtools::document()` should handle this automatically.

### 10. NEWS.md

Add entry at top for next version:
```markdown
* `chat_openai()` has been renamed to `chat_openai_compatible()` to clarify it's for OpenAI-compatible APIs, not the official OpenAI API. The `base_url` parameter is now required (#801).
* `chat_openai_responses()` has been renamed to `chat_openai()`. This is now the main function for using OpenAI's official API via the responses endpoint.
```

### 11. _pkgdown.yml

No work needed.

### 12. Class Inheritance Considerations

Rename classes to match new APIs:
- `ProviderOpenAI` → `ProviderOpenAICompatible`
- `ProviderOpenAIResponses` → `ProviderOpenAI`

This affects all providers that inherit from `ProviderOpenAI`:
- Update their `parent = ProviderOpenAI` to `parent = ProviderOpenAICompatible`
- Providers affected: Groq, Ollama, Perplexity, Huggingface, and any others that currently inherit from ProviderOpenAI

## Testing Strategy

1. Run `devtools::document()` to regenerate man pages and NAMESPACE
2. Run `devtools::test(reporter = "check")` to run all tests
3. Update snapshot files as needed
4. Run `pkgdown::check_pkgdown()` to verify all topics in reference
5. Verify examples in vignettes still work
6. Check that cross-references work correctly

The vcr cassettes will fail; but Hadley will take care of that.

## Files to Change Summary

### Core Implementation (4 files)
- R/provider-openai.R → R/provider-openai-compatible.R
- R/provider-openai-responses.R → R/provider-openai.R
- tests/testthat/test-provider-openai.R → tests/testthat/test-provider-openai-compatible.R
- tests/testthat/test-provider-openai-responses.R → tests/testthat/test-provider-openai.R

### Provider Documentation (19 files)
- All files using `@inheritParams chat_openai` or referencing the function

### Test Files (9+ files)
- test-provider-openai.R → test-provider-openai-compatible.R
- test-provider-openai-responses.R → test-provider-openai.R
- test-parallel-chat.R
- test-batch-chat.R
- test-content.R
- test-chat.R
- test-chat-tools.R
- test-chat-structured.R
- test-utils-auth.R
- test-chat-utils.R

### Snapshot Files (2 files)
- _snaps/provider-openai.md → _snaps/provider-openai-compatible.md
- _snaps/provider-openai-responses.md → _snaps/provider-openai.md

### Documentation Files (6 vignettes + 1 README)
- vignettes/ellmer.Rmd
- vignettes/tool-calling.Rmd
- vignettes/streaming-async.Rmd
- vignettes/programming.Rmd
- vignettes/prompt-design.Rmd
- vignettes/structured-data.Rmd
- README.Rmd

### Source Code Docs (5 files)
- R/live.R
- R/content-image.R
- R/batch-chat.R
- R/parallel-chat.R
- R/tools-def-auto.R

### Other (1 file)
- NEWS.md

## Estimated Total Files: ~45 files

## Risks and Mitigations

### High Risk
1. **Breaking change for users** - All code using `chat_openai()` will break
   - Mitigation: Clear NEWS.md entry, consider blog post/announcement

2. **Documentation inheritance cascade** - 19 files use `@inheritParams`
   - Mitigation: Careful find-replace, thorough testing of `devtools::document()`

### Medium Risk
1. **Test disruption** - Many tests use `chat_openai_test()`
   - Mitigation: Update systematically, run tests frequently

2. **Snapshot mismatches** - Function names in snapshots will change
   - Mitigation: Review and approve snapshot updates carefully

### Low Risk
1. **Cross-reference links** - `@family chatbots` should auto-update
   - Mitigation: Verify after `devtools::document()`

## Open Questions

1. Should general tests use `chat_openai_test()` (responses API) or `chat_openai_compatible_test()`?
   - **Recommendation:** Use `chat_openai_test()` (responses API) as the primary test function since it's the main OpenAI interface

2. Should we update `OPENAI_BASE_URL` environment variable logic in the compatible version?
   - Current behavior: Falls back to `OPENAI_BASE_URL` if set
   - Keep this in `chat_openai_compatible()` but drop from `chat_openai()`

3. How to handle the `service_tier` parameter that's unique to responses API?
   - It's only in `chat_openai()` (responses), not in `chat_openai_compatible()`
   - No work needed

## Success Criteria

- [ ] All functions renamed correctly
- [ ] `base_url` parameter is required in `chat_openai_compatible()`
- [ ] Documentation clearly distinguishes compatible API vs official OpenAI API
- [ ] All tests pass
- [ ] All vignettes knit successfully
- [ ] `pkgdown::check_pkgdown()` passes
- [ ] README.md updated
- [ ] NEWS.md entry added
- [ ] No broken cross-references in documentation
- [ ] Snapshot tests updated and reviewed
