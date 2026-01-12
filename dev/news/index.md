# Changelog

## ellmer (development version)

- ellmer will now distinguish text content from thinking content while
  streaming, allowing downstream packages like shinychat to provide
  specific UI for thinking content
  ([@simonpcouch](https://github.com/simonpcouch),
  [\#909](https://github.com/tidyverse/ellmer/issues/909)).
- [`chat_github()`](https://ellmer.tidyverse.org/dev/reference/chat_github.md)
  now uses
  [`chat_openai_compatible()`](https://ellmer.tidyverse.org/dev/reference/chat_openai_compatible.md)
  for improved compatibility, and
  [`models_github()`](https://ellmer.tidyverse.org/dev/reference/chat_github.md)
  now supports custom `base_url` configuration
  ([@D-M4rk](https://github.com/D-M4rk),
  [\#877](https://github.com/tidyverse/ellmer/issues/877)).
- [`chat_ollama()`](https://ellmer.tidyverse.org/dev/reference/chat_ollama.md)
  now contains a slot for `top_k` within the `params` argument
  ([@frankiethull](https://github.com/frankiethull)).

## ellmer 0.4.0

CRAN release: 2025-11-15

### Lifecycle changes

- [`chat_claude()`](https://ellmer.tidyverse.org/dev/reference/chat_anthropic.md)
  is no longer deprecated and is an alias for
  [`chat_anthropic()`](https://ellmer.tidyverse.org/dev/reference/chat_anthropic.md),
  reflecting Anthropic’s recent rebranding of developer tools under the
  Claude name ([\#758](https://github.com/tidyverse/ellmer/issues/758)).
  [`models_claude()`](https://ellmer.tidyverse.org/dev/reference/chat_anthropic.md)
  is now an alias for
  [`models_anthropic()`](https://ellmer.tidyverse.org/dev/reference/chat_anthropic.md).
- [`parallel_chat()`](https://ellmer.tidyverse.org/dev/reference/parallel_chat.md)
  and
  [`batch_chat()`](https://ellmer.tidyverse.org/dev/reference/batch_chat.md)
  are no longer experimental.
- The following deprecated functions/arguments/methods have now been
  removed:
  - `Chat$extract_data()` -\> `chat$chat_structured()` (0.2.0)
  - `Chat$extract_data_async()` -\> `chat$chat_structured_async()`
    (0.2.0)
  - `chat_anthropic(max_tokens)` -\> `chat_anthropic(params)` (0.2.0)
  - `chat_azure()` -\>
    [`chat_azure_openai()`](https://ellmer.tidyverse.org/dev/reference/chat_azure_openai.md)
    (0.2.0)
  - `chat_azure_openai(token)` (0.1.1)
  - `chat_bedrock()` -\>
    [`chat_aws_bedrock()`](https://ellmer.tidyverse.org/dev/reference/chat_aws_bedrock.md)
    (0.2.0)
  - [`chat_claude()`](https://ellmer.tidyverse.org/dev/reference/chat_anthropic.md)
    -\>
    [`chat_anthropic()`](https://ellmer.tidyverse.org/dev/reference/chat_anthropic.md)
    (0.2.0)
  - `chat_cortex()` -\>
    [`chat_snowflake()`](https://ellmer.tidyverse.org/dev/reference/chat_snowflake.md)
    (0.2.0)
  - `chat_gemini()` -\>
    [`chat_google_gemini()`](https://ellmer.tidyverse.org/dev/reference/chat_google_gemini.md)
    (0.2.0)
  - `chat_openai(seed)` -\> `chat_openai(params)` (0.2.0)
  - `create_tool_def(model)` -\> `create_tool_def(chat)` (0.2.0)

### New features

- `batch_*()` no longer hashes properties of the provider besides the
  `name`, `model`, and `base_url`. This should provide some protection
  from accidentally reusing the same `.json` file with different
  providers, while still allowing you to use the same batch file across
  ellmer versions. It also has a new `ignore_hash` argument that allows
  you to opt out of the check if you’re confident the difference only
  arises because ellmer itself has changed.
- [`chat_claude()`](https://ellmer.tidyverse.org/dev/reference/chat_anthropic.md)
  gains new `cache` parameter to control caching. By default it is set
  to “5m”. This should (on average) reduce the cost of your chats
  ([\#584](https://github.com/tidyverse/ellmer/issues/584)).
- [`chat_openai()`](https://ellmer.tidyverse.org/dev/reference/chat_openai.md)
  now uses OpenAI’s responses endpoint
  ([\#365](https://github.com/tidyverse/ellmer/issues/365),
  [\#801](https://github.com/tidyverse/ellmer/issues/801)). This is
  their recommended endpoint and gives more access to built-in tools.
- [`chat_openai_compatible()`](https://ellmer.tidyverse.org/dev/reference/chat_openai_compatible.md)
  replaces
  [`chat_openai()`](https://ellmer.tidyverse.org/dev/reference/chat_openai.md)
  as the interface to use for OpenAI-compatible APIs, and
  [`chat_openai()`](https://ellmer.tidyverse.org/dev/reference/chat_openai.md)
  is reserved for the official OpenAI API. Unlike previous versions of
  [`chat_openai()`](https://ellmer.tidyverse.org/dev/reference/chat_openai.md),
  the `base_url` parameter is now required
  ([\#801](https://github.com/tidyverse/ellmer/issues/801)).
- `chat_*()` functions now use a `credentials` function instead of an
  `api_key` ([\#613](https://github.com/tidyverse/ellmer/issues/613)).
  This means that API keys are never stored in the chat object (which
  might be saved to disk), but are instead retrieved on demand as
  needed. You generally shouldn’t need to use the `credentials`
  argument, but when you do, you should use it to dynamically retrieve
  the API key from some other source (i.e. never inline a secret
  directly into a function call).
- New set of `claude_file_()` functions for managing file uploads with
  Claude ([@dcomputing](https://github.com/dcomputing),
  [\#761](https://github.com/tidyverse/ellmer/issues/761)).
- ellmer now supports a variety of built-in web search and fetch tools
  ([\#578](https://github.com/tidyverse/ellmer/issues/578)):
  - [`claude_tool_web_search()`](https://ellmer.tidyverse.org/dev/reference/claude_tool_web_search.md)
    and
    [`claude_tool_web_fetch()`](https://ellmer.tidyverse.org/dev/reference/claude_tool_web_fetch.md)
    for Claude.
  - [`google_tool_web_search()`](https://ellmer.tidyverse.org/dev/reference/google_tool_web_search.md)
    and
    [`google_tool_web_fetch()`](https://ellmer.tidyverse.org/dev/reference/google_tool_web_fetch.md)
    for Gemini.
  - [`openai_tool_web_search()`](https://ellmer.tidyverse.org/dev/reference/openai_tool_web_search.md)
    for OpenAI. If you want to do web fetch for other providers, you
    could use `btw::btw_tool_web_read_url()`.
- [`parallel_chat()`](https://ellmer.tidyverse.org/dev/reference/parallel_chat.md)
  and friends now have a more permissive attitude to errors. By default,
  they will now return when hitting the first error (rather than
  erroring), and you can control this behaviour with the `on_error`
  argument. Or if you interrupt the job, it will finish up current
  requests and then return all the work done so far. The main downside
  of this work is that the output of
  [`parallel_chat()`](https://ellmer.tidyverse.org/dev/reference/parallel_chat.md)
  is more complex: it is now a mix of `Chat` objects, error objects, and
  `NULL` ([\#628](https://github.com/tidyverse/ellmer/issues/628)).
- [`parallel_chat_structured()`](https://ellmer.tidyverse.org/dev/reference/parallel_chat.md)
  no longer errors if some results fail to parse. Instead it warns, and
  the corresponding rows will be filled in with the appropriate missing
  values ([\#628](https://github.com/tidyverse/ellmer/issues/628)).
- New `schema_df()` to describe the schema of a data frame to an LLM
  ([\#744](https://github.com/tidyverse/ellmer/issues/744)).
- [`tool()`](https://ellmer.tidyverse.org/dev/reference/tool.md)s can
  now return image or PDF content types, with
  [`content_image_file()`](https://ellmer.tidyverse.org/dev/reference/content_image_url.md)
  or `content_image_pdf()`
  ([\#735](https://github.com/tidyverse/ellmer/issues/735)).
- [`params()`](https://ellmer.tidyverse.org/dev/reference/params.md)
  gains new `reasoning_effort` and `reasoning_tokens` so you can control
  the amount of effort a model spends on thinking. Initial support is
  provided for
  [`chat_claude()`](https://ellmer.tidyverse.org/dev/reference/chat_anthropic.md),
  [`chat_google_gemini()`](https://ellmer.tidyverse.org/dev/reference/chat_google_gemini.md),
  and
  [`chat_openai()`](https://ellmer.tidyverse.org/dev/reference/chat_openai.md)
  ([\#720](https://github.com/tidyverse/ellmer/issues/720)).
- New
  [`type_ignore()`](https://ellmer.tidyverse.org/dev/reference/type_boolean.md)
  allows you to specify that a tool argument should not be provided by
  the LLM when the R function has a suitable default value
  ([\#764](https://github.com/tidyverse/ellmer/issues/764)).

### Minor improvements and bug fixes

- Updated pricing data
  ([\#790](https://github.com/tidyverse/ellmer/issues/790)).
- `AssistantTurn`s now have a `@duration` slot, containing the total
  time to complete the request
  ([@simonpcouch](https://github.com/simonpcouch),
  [\#798](https://github.com/tidyverse/ellmer/issues/798)).
- [`batch_chat()`](https://ellmer.tidyverse.org/dev/reference/batch_chat.md)
  logs tokens once, on retrieval
  ([\#743](https://github.com/tidyverse/ellmer/issues/743)).
- [`batch_chat()`](https://ellmer.tidyverse.org/dev/reference/batch_chat.md)
  now retrieves failed results for
  [`chat_openai()`](https://ellmer.tidyverse.org/dev/reference/chat_openai.md)
  ([\#830](https://github.com/tidyverse/ellmer/issues/830)) and
  gracefully handles invalid JSON
  ([\#845](https://github.com/tidyverse/ellmer/issues/845)).
- [`batch_chat()`](https://ellmer.tidyverse.org/dev/reference/batch_chat.md)
  now works once more for
  [`chat_anthropic()`](https://ellmer.tidyverse.org/dev/reference/chat_anthropic.md)
  ([\#835](https://github.com/tidyverse/ellmer/issues/835)).
- `batch_chat_*()` and `parallel_chat_*()` now accept a string as the
  chat object, following the same rules as
  [`chat()`](https://ellmer.tidyverse.org/dev/reference/chat-any.md)
  ([\#677](https://github.com/tidyverse/ellmer/issues/677)).
- [`chat_claude()`](https://ellmer.tidyverse.org/dev/reference/chat_anthropic.md)
  and
  [`chat_aws_bedrock()`](https://ellmer.tidyverse.org/dev/reference/chat_aws_bedrock.md)
  now default to Claude Sonnet 4.5
  ([\#800](https://github.com/tidyverse/ellmer/issues/800)).
- [`chat_databricks()`](https://ellmer.tidyverse.org/dev/reference/chat_databricks.md)
  lifts many of its restrictions now that Databricks’ API is more OpenAI
  compatible ([\#757](https://github.com/tidyverse/ellmer/issues/757)).
- [`chat_google_gemini()`](https://ellmer.tidyverse.org/dev/reference/chat_google_gemini.md)
  and
  [`chat_openai()`](https://ellmer.tidyverse.org/dev/reference/chat_openai.md)
  support image generation
  ([\#368](https://github.com/tidyverse/ellmer/issues/368)).
- [`chat_google_gemini()`](https://ellmer.tidyverse.org/dev/reference/chat_google_gemini.md)
  has an experimental fallback interactive OAuth flow, if you’re in an
  interactive session and no other authentication options can be found
  ([\#680](https://github.com/tidyverse/ellmer/issues/680)).
- [`chat_groq()`](https://ellmer.tidyverse.org/dev/reference/chat_groq.md)
  now defaults to llama-3.1-8b-instant.
- [`chat_openai()`](https://ellmer.tidyverse.org/dev/reference/chat_openai.md)
  gains a `service_tier` argument
  ([\#712](https://github.com/tidyverse/ellmer/issues/712)).
- [`chat_portkey()`](https://ellmer.tidyverse.org/dev/reference/chat_portkey.md)
  now requires you to supply a model
  ([\#786](https://github.com/tidyverse/ellmer/issues/786)).
- `chat_portkey(virtual_key)` no longer needs to be supplied; instead
  Portkey recommends including the virtual key/provider in the `model`
  ([\#786](https://github.com/tidyverse/ellmer/issues/786)).
- `Chat$chat()`, `Chat$stream()`, and similar methods now add empty tool
  results when a the chat is interrupted during a tool call loop,
  allowing the conversation to be resumed without causing an API error
  ([\#840](https://github.com/tidyverse/ellmer/issues/840)).
- `Chat$chat_structured()` and friends now only warn if multiple JSON
  payloads found (instead of erroring)
  ([@kbenoit](https://github.com/kbenoit),
  [\#732](https://github.com/tidyverse/ellmer/issues/732)).
- `Chat$get_tokens()` gives a brief description of the turn contents to
  make it easier to see which turn tokens are spent on
  ([\#618](https://github.com/tidyverse/ellmer/issues/618)) and also
  returns the cost
  ([\#824](https://github.com/tidyverse/ellmer/issues/824)). It now
  returns one row for each assistant turn, better representing the
  underlying data received from LLM APIs. Similarly, the
  [`print()`](https://rdrr.io/r/base/print.html) method now reports
  costs on each assistant turn, rather than trying to parse out
  individual costs.
- [`interpolate_package()`](https://ellmer.tidyverse.org/dev/reference/interpolate.md)
  now provides an informative error if the requested prompt file is not
  found in the package’s `prompts/` directory
  ([\#763](https://github.com/tidyverse/ellmer/issues/763)) and now
  works with in-development packages loaded with devtools
  ([\#766](https://github.com/tidyverse/ellmer/issues/766)).
- [`models_mistral()`](https://ellmer.tidyverse.org/dev/reference/chat_mistral.md)
  lists available models ([@rplsmn](https://github.com/rplsmn),
  [\#750](https://github.com/tidyverse/ellmer/issues/750)).
- [`models_ollama()`](https://ellmer.tidyverse.org/dev/reference/chat_ollama.md)
  was fixed to correctly query model capabilities from remote Ollama
  servers ([\#746](https://github.com/tidyverse/ellmer/issues/746)).
- [`chat_ollama()`](https://ellmer.tidyverse.org/dev/reference/chat_ollama.md)
  now uses `credentials` when checking if Ollama is available and
  [`models_ollama()`](https://ellmer.tidyverse.org/dev/reference/chat_ollama.md)
  now has a `credentials` argument. This is useful when accessing Ollama
  servers that require authentication
  ([@AdaemmerP](https://github.com/AdaemmerP),
  [\#863](https://github.com/tidyverse/ellmer/issues/863)).
- [`parallel_chat_structured()`](https://ellmer.tidyverse.org/dev/reference/parallel_chat.md)
  now returns a tibble, since this does a better job of printing more
  complex data frames
  ([\#787](https://github.com/tidyverse/ellmer/issues/787)).

## ellmer 0.3.2

CRAN release: 2025-09-03

- [`chat()`](https://ellmer.tidyverse.org/dev/reference/chat-any.md) is
  now compatible with most `chat_` functions
  ([\#699](https://github.com/tidyverse/ellmer/issues/699)).
  - [`chat_aws_bedrock()`](https://ellmer.tidyverse.org/dev/reference/chat_aws_bedrock.md),
    [`chat_databricks()`](https://ellmer.tidyverse.org/dev/reference/chat_databricks.md),
    [`chat_deepseek()`](https://ellmer.tidyverse.org/dev/reference/chat_deepseek.md),
    [`chat_github()`](https://ellmer.tidyverse.org/dev/reference/chat_github.md),
    [`chat_groq()`](https://ellmer.tidyverse.org/dev/reference/chat_groq.md),
    [`chat_ollama()`](https://ellmer.tidyverse.org/dev/reference/chat_ollama.md),
    [`chat_openrouter()`](https://ellmer.tidyverse.org/dev/reference/chat_openrouter.md),
    [`chat_perplexity()`](https://ellmer.tidyverse.org/dev/reference/chat_perplexity.md),
    and
    [`chat_vllm()`](https://ellmer.tidyverse.org/dev/reference/chat_vllm.md)
    now support a `params` argument that accepts common model parameters
    from
    [`params()`](https://ellmer.tidyverse.org/dev/reference/params.md).
  - The `deployment_id` argument in
    [`chat_azure_openai()`](https://ellmer.tidyverse.org/dev/reference/chat_azure_openai.md)
    was deprecated and replaced with `model` to better align with other
    providers.
- [`chat_openai()`](https://ellmer.tidyverse.org/dev/reference/chat_openai.md)
  now correctly maps `max_tokens` and `top_k` from
  [`params()`](https://ellmer.tidyverse.org/dev/reference/params.md) to
  the OpenAI API parameters
  ([\#699](https://github.com/tidyverse/ellmer/issues/699)).

## ellmer 0.3.1

CRAN release: 2025-08-24

- [`chat_anthropic()`](https://ellmer.tidyverse.org/dev/reference/chat_anthropic.md)
  drops empty assistant turns to avoid API errors
  ([\#710](https://github.com/tidyverse/ellmer/issues/710)).

- [`chat_github()`](https://ellmer.tidyverse.org/dev/reference/chat_github.md)
  now uses the `https://models.github.ai/inference` endpoint and
  [`chat()`](https://ellmer.tidyverse.org/dev/reference/chat-any.md)
  supports GitHub models in the format `chat("github/openai/gpt-4.1")`
  ([\#726](https://github.com/tidyverse/ellmer/issues/726)).

- [`chat_google_vertex()`](https://ellmer.tidyverse.org/dev/reference/chat_google_gemini.md)
  authentication was fixed using broader scope
  ([\#704](https://github.com/tidyverse/ellmer/issues/704),
  [@netique](https://github.com/netique))

- [`chat_google_vertex()`](https://ellmer.tidyverse.org/dev/reference/chat_google_gemini.md)
  can now use `global` project location
  ([\#704](https://github.com/tidyverse/ellmer/issues/704),
  [@netique](https://github.com/netique))

- [`chat_openai()`](https://ellmer.tidyverse.org/dev/reference/chat_openai.md)
  now uses `OPENAI_BASE_URL`, if set, for the `base_url`. Similarly,
  [`chat_ollama()`](https://ellmer.tidyverse.org/dev/reference/chat_ollama.md)
  also uses `OLLAMA_BASE_URL` if set
  ([\#713](https://github.com/tidyverse/ellmer/issues/713)).

- [`contents_record()`](https://ellmer.tidyverse.org/dev/reference/contents_record.md)
  and
  [`contents_replay()`](https://ellmer.tidyverse.org/dev/reference/contents_record.md)
  now record and replay custom classes that extend ellmer’s `Turn` or
  `Content` classes
  ([\#689](https://github.com/tidyverse/ellmer/issues/689)).
  [`contents_replay()`](https://ellmer.tidyverse.org/dev/reference/contents_record.md)
  now also restores the tool definition in `ContentToolResult` objects
  (in `@request@tool`)
  ([\#693](https://github.com/tidyverse/ellmer/issues/693)).

- [`chat_snowflake()`](https://ellmer.tidyverse.org/dev/reference/chat_snowflake.md)
  now supports Privatelink accounts
  ([\#694](https://github.com/tidyverse/ellmer/issues/694),
  [@robert-norberg](https://github.com/robert-norberg)). and works
  against Snowflake’s latest API changes
  ([\#692](https://github.com/tidyverse/ellmer/issues/692),
  [@robert-norberg](https://github.com/robert-norberg)).

- [`models_google_vertex()`](https://ellmer.tidyverse.org/dev/reference/chat_google_gemini.md)
  works once again
  ([\#704](https://github.com/tidyverse/ellmer/issues/704),
  [@netique](https://github.com/netique))

- In the `value_turn()` method for OpenAI providers, `usage` is checked
  if `NULL` before logging tokens to avoid errors when streaming with
  some OpenAI-compatible services
  ([\#706](https://github.com/tidyverse/ellmer/issues/706),
  [@stevegbrooks](https://github.com/stevegbrooks)).

## ellmer 0.3.0

CRAN release: 2025-07-24

### New features

- New [`chat()`](https://ellmer.tidyverse.org/dev/reference/chat-any.md)
  allows you to chat with any provider using a string like
  `chat("anthropic")` or `chat("openai/gpt-4.1-nano")`
  ([\#361](https://github.com/tidyverse/ellmer/issues/361)).

- [`tool()`](https://ellmer.tidyverse.org/dev/reference/tool.md) has a
  simpler specification: you now specify the `name`, `description`, and
  `arguments`. I have done my best to deprecate old usage and give clear
  errors, but I have likely missed a few edge cases. I apologize for any
  pain that this causes, but I’m convinced that it is going to make tool
  usage easier and clearer in the long run. If you have many calls to
  convert, [`?tool`](https://ellmer.tidyverse.org/dev/reference/tool.md)
  contains a prompt that will help you use an LLM to convert them
  ([\#603](https://github.com/tidyverse/ellmer/issues/603)). It also now
  returns a function so that you can call it (and/or export it from your
  package) ([\#602](https://github.com/tidyverse/ellmer/issues/602)).

- [`type_array()`](https://ellmer.tidyverse.org/dev/reference/type_boolean.md)
  and
  [`type_enum()`](https://ellmer.tidyverse.org/dev/reference/type_boolean.md)
  now have the description as the second argument and `items`/`values`
  as the first. This makes them easier to use in the common case where
  the description isn’t necessary
  ([\#610](https://github.com/tidyverse/ellmer/issues/610)).

- ellmer now retries requests up to 3 times, controllable with
  `option(ellmer_max_tries)`, and will retry if the connection fails
  (rather than just if the request itself returns a transient error).
  The default timeout, controlled by `option(ellmer_timeout_s)`, now
  applies to the initial connection phase. Together, these changes
  should make it much more likely for ellmer requests to succeed.

- New
  [`parallel_chat_text()`](https://ellmer.tidyverse.org/dev/reference/parallel_chat.md)
  and
  [`batch_chat_text()`](https://ellmer.tidyverse.org/dev/reference/batch_chat.md)
  make it easier to just get the text response from multiple prompts
  ([\#510](https://github.com/tidyverse/ellmer/issues/510)).

- ellmer’s cost estimates are considerably improved.
  [`chat_openai()`](https://ellmer.tidyverse.org/dev/reference/chat_openai.md),
  [`chat_google_gemini()`](https://ellmer.tidyverse.org/dev/reference/chat_google_gemini.md),
  and
  [`chat_anthropic()`](https://ellmer.tidyverse.org/dev/reference/chat_anthropic.md)
  capture the number of cached input tokens. This is primarily useful
  for OpenAI and Gemini since both offer automatic caching, yielding
  improved cost estimates
  ([\#466](https://github.com/tidyverse/ellmer/issues/466)). We also
  have a better source of pricing data, LiteLLM. This considerably
  expands the number of providers and models that include cost
  information ([\#659](https://github.com/tidyverse/ellmer/issues/659)).

### Bug fixes and minor improvements

- The new `ellmer_echo` option controls the default value for `echo`.
- [`batch_chat_structured()`](https://ellmer.tidyverse.org/dev/reference/batch_chat.md)
  provides clear messaging when prompts/path/provider don’t match
  ([\#599](https://github.com/tidyverse/ellmer/issues/599)).
- [`chat_aws_bedrock()`](https://ellmer.tidyverse.org/dev/reference/chat_aws_bedrock.md)
  allows you to set the `base_url()`
  ([\#441](https://github.com/tidyverse/ellmer/issues/441)).
- [`chat_aws_bedrock()`](https://ellmer.tidyverse.org/dev/reference/chat_aws_bedrock.md),
  [`chat_google_gemini()`](https://ellmer.tidyverse.org/dev/reference/chat_google_gemini.md),
  [`chat_ollama()`](https://ellmer.tidyverse.org/dev/reference/chat_ollama.md),
  and
  [`chat_vllm()`](https://ellmer.tidyverse.org/dev/reference/chat_vllm.md)
  use a more robust method to generate model URLs from the `base_url`
  ([\#593](https://github.com/tidyverse/ellmer/issues/593),
  [@benyake](https://github.com/benyake)).
- `chat_cortex_analyst()` is deprecated; please use
  [`chat_snowflake()`](https://ellmer.tidyverse.org/dev/reference/chat_snowflake.md)
  instead ([\#640](https://github.com/tidyverse/ellmer/issues/640)).
- [`chat_github()`](https://ellmer.tidyverse.org/dev/reference/chat_github.md)
  (and other OpenAI extensions) no longer warn about `seed`
  ([\#574](https://github.com/tidyverse/ellmer/issues/574)).
- [`chat_google_gemini()`](https://ellmer.tidyverse.org/dev/reference/chat_google_gemini.md)
  and
  [`chat_google_vertex()`](https://ellmer.tidyverse.org/dev/reference/chat_google_gemini.md)
  default to Gemini 2.5 flash
  ([\#576](https://github.com/tidyverse/ellmer/issues/576)).
- [`chat_huggingface()`](https://ellmer.tidyverse.org/dev/reference/chat_huggingface.md)
  works much better.
- [`chat_openai()`](https://ellmer.tidyverse.org/dev/reference/chat_openai.md)
  supports `content_pdf_()`
  ([\#650](https://github.com/tidyverse/ellmer/issues/650)).
- [`chat_portkey()`](https://ellmer.tidyverse.org/dev/reference/chat_portkey.md)
  works once again, and reads the virtual API key from the
  `PORTKEY_VIRTUAL_KEY` env var
  ([\#588](https://github.com/tidyverse/ellmer/issues/588)).
- [`chat_snowflake()`](https://ellmer.tidyverse.org/dev/reference/chat_snowflake.md)
  works with tool calling
  ([\#557](https://github.com/tidyverse/ellmer/issues/557),
  [@atheriel](https://github.com/atheriel)).
- `Chat$chat_structured()` and friends no longer unnecessarily wrap
  [`type_object()`](https://ellmer.tidyverse.org/dev/reference/type_boolean.md)
  for
  [`chat_openai()`](https://ellmer.tidyverse.org/dev/reference/chat_openai.md)
  ([\#671](https://github.com/tidyverse/ellmer/issues/671)).
- `Chat$chat_structured()` suppresses tool use. If you need to use tools
  and structured data together, first use `$chat()` for any needed
  tools, then `$chat_structured()` to extract the data you need.
- `Chat$chat_structured()` no longer requires a prompt (since it may be
  obvious from the context)
  ([\#570](https://github.com/tidyverse/ellmer/issues/570)).
- `Chat$register_tool()` shows a message when you replace an existing
  tool ([\#625](https://github.com/tidyverse/ellmer/issues/625)).
- [`contents_record()`](https://ellmer.tidyverse.org/dev/reference/contents_record.md)
  and
  [`contents_replay()`](https://ellmer.tidyverse.org/dev/reference/contents_record.md)
  record and replay `Turn` related information from a `Chat` instance
  ([\#502](https://github.com/tidyverse/ellmer/issues/502)). These
  methods can be used for bookmarking within {shinychat}.
- [`models_github()`](https://ellmer.tidyverse.org/dev/reference/chat_github.md)
  lists models for
  [`chat_github()`](https://ellmer.tidyverse.org/dev/reference/chat_github.md)
  ([\#561](https://github.com/tidyverse/ellmer/issues/561)).
- [`models_ollama()`](https://ellmer.tidyverse.org/dev/reference/chat_ollama.md)
  includes a `capabilities` column with a comma-separated list of model
  capabilities
  ([\#623](https://github.com/tidyverse/ellmer/issues/623)).
- [`parallel_chat()`](https://ellmer.tidyverse.org/dev/reference/parallel_chat.md)
  and friends accept lists of `Content` objects in the `prompt`
  ([\#597](https://github.com/tidyverse/ellmer/issues/597),
  [@thisisnic](https://github.com/thisisnic)).
- Tool requests show converted arguments when printed
  ([\#517](https://github.com/tidyverse/ellmer/issues/517)).
- [`tool()`](https://ellmer.tidyverse.org/dev/reference/tool.md) checks
  that the `name` is valid
  ([\#625](https://github.com/tidyverse/ellmer/issues/625)).

## ellmer 0.2.1

CRAN release: 2025-06-03

- When you save a `Chat` object to disk, API keys are This means that
  you can no longer easily resume a chat you’ve saved on disk (we’ll
  figure this out in a future release) but ensures that you never
  accidentally save your secret key in an RDS file
  ([\#534](https://github.com/tidyverse/ellmer/issues/534)).

- [`chat_anthropic()`](https://ellmer.tidyverse.org/dev/reference/chat_anthropic.md)
  now defaults to Claude Sonnet 4, and I’ve added pricing information
  for the latest generation of Claude models.

- [`chat_databricks()`](https://ellmer.tidyverse.org/dev/reference/chat_databricks.md)
  now picks up on Databricks workspace URLs set in the configuration
  file, which should improve compatibility with the Databricks CLI
  ([\#521](https://github.com/tidyverse/ellmer/issues/521),
  [@atheriel](https://github.com/atheriel)). It now also supports tool
  calling ([\#548](https://github.com/tidyverse/ellmer/issues/548),
  [@atheriel](https://github.com/atheriel)).

- [`chat_snowflake()`](https://ellmer.tidyverse.org/dev/reference/chat_snowflake.md)
  no longer streams answers that include a mysterious
  `list(type = "text", text = "")` trailer
  ([\#533](https://github.com/tidyverse/ellmer/issues/533),
  [@atheriel](https://github.com/atheriel)). It now parses streaming
  outputs correctly into turns
  ([\#542](https://github.com/tidyverse/ellmer/issues/542)), supports
  structured ouputs
  ([\#544](https://github.com/tidyverse/ellmer/issues/544)), and
  standard model parameters
  ([\#545](https://github.com/tidyverse/ellmer/issues/545),
  [@atheriel](https://github.com/atheriel)).

- [`chat_snowflake()`](https://ellmer.tidyverse.org/dev/reference/chat_snowflake.md)
  and
  [`chat_databricks()`](https://ellmer.tidyverse.org/dev/reference/chat_databricks.md)
  now default to Claude Sonnet 3.7, the same default as
  [`chat_anthropic()`](https://ellmer.tidyverse.org/dev/reference/chat_anthropic.md)
  ([\#539](https://github.com/tidyverse/ellmer/issues/539) and
  [\#546](https://github.com/tidyverse/ellmer/issues/546),
  [@atheriel](https://github.com/atheriel)).

- [`type_from_schema()`](https://ellmer.tidyverse.org/dev/reference/type_boolean.md)
  lets you to use pre-existing JSON schemas in structured chats
  ([\#133](https://github.com/tidyverse/ellmer/issues/133),
  [@hafen](https://github.com/hafen))

## ellmer 0.2.0

CRAN release: 2025-05-17

### Breaking changes

- We have made a number of refinements to the way ellmer converts JSON
  to R data structures. These are breaking changes, although we don’t
  expect them to affect much code in the wild. Most importantly, tools
  are now invoked with their inputs coerced to standard R data
  structures ([\#461](https://github.com/tidyverse/ellmer/issues/461));
  opt-out by setting `convert = FALSE` in
  [`tool()`](https://ellmer.tidyverse.org/dev/reference/tool.md).

  Additionally ellmer now converts `NULL` to `NA` for
  [`type_boolean()`](https://ellmer.tidyverse.org/dev/reference/type_boolean.md),
  [`type_integer()`](https://ellmer.tidyverse.org/dev/reference/type_boolean.md),
  [`type_number()`](https://ellmer.tidyverse.org/dev/reference/type_boolean.md),
  and
  [`type_string()`](https://ellmer.tidyverse.org/dev/reference/type_boolean.md)
  ([\#445](https://github.com/tidyverse/ellmer/issues/445)), and does a
  better job with arrays when `required = FALSE`
  ([\#384](https://github.com/tidyverse/ellmer/issues/384)).

- `chat_` functions no longer have a `turn` argument. If you need to set
  the turns, you can now use `Chat$set_turns()`
  ([\#427](https://github.com/tidyverse/ellmer/issues/427)).
  Additionally, `Chat$tokens()` has been renamed to `Chat$get_tokens()`
  and returns a data frame of tokens, correctly aligned to the
  individual turn. The print method now uses this to show how many
  input/output tokens were used by each turn
  ([\#354](https://github.com/tidyverse/ellmer/issues/354)).

### New features

- Two new interfaces help you do multiple chats with a single function
  call:

  - [`batch_chat()`](https://ellmer.tidyverse.org/dev/reference/batch_chat.md)
    and
    [`batch_chat_structured()`](https://ellmer.tidyverse.org/dev/reference/batch_chat.md)
    allow you to submit multiple chats to OpenAI and Anthropic’s batched
    interfaces. These only guarantee a response within 24 hours, but are
    50% of the price of regular requests
    ([\#143](https://github.com/tidyverse/ellmer/issues/143)).

  - [`parallel_chat()`](https://ellmer.tidyverse.org/dev/reference/parallel_chat.md)
    and
    [`parallel_chat_structured()`](https://ellmer.tidyverse.org/dev/reference/parallel_chat.md)
    work with any provider and allow you to submit multiple chats in
    parallel ([\#143](https://github.com/tidyverse/ellmer/issues/143)).
    This doesn’t give you any cost savings, but it’s can be much, much
    faster.

  This new family of functions is experimental because I’m not 100% sure
  that the shape of the user interface is correct, particularly as it
  pertains to handling errors.

- [`google_upload()`](https://ellmer.tidyverse.org/dev/reference/google_upload.md)
  lets you upload files to Google Gemini or Vertex AI
  ([\#310](https://github.com/tidyverse/ellmer/issues/310)). This allows
  you to work with videos, PDFs, and other large files with Gemini.

- [`models_google_gemini()`](https://ellmer.tidyverse.org/dev/reference/chat_google_gemini.md),
  [`models_anthropic()`](https://ellmer.tidyverse.org/dev/reference/chat_anthropic.md),
  [`models_openai()`](https://ellmer.tidyverse.org/dev/reference/chat_openai.md),
  [`models_aws_bedrock()`](https://ellmer.tidyverse.org/dev/reference/chat_aws_bedrock.md),
  [`models_ollama()`](https://ellmer.tidyverse.org/dev/reference/chat_ollama.md)
  and
  [`models_vllm()`](https://ellmer.tidyverse.org/dev/reference/chat_vllm.md),
  list available models for Google Gemini, Anthropic, OpenAI, AWS
  Bedrock, Ollama, and VLLM respectively. Different providers return
  different metadata so they are only guaranteed to return a data frame
  with at least an `id` column
  ([\#296](https://github.com/tidyverse/ellmer/issues/296)). Where
  possible (currently for Gemini, Anthropic, and OpenAI) we include
  known token prices (per million tokens).

- [`interpolate()`](https://ellmer.tidyverse.org/dev/reference/interpolate.md)
  and friends are now vectorised so you can generate multiple prompts
  for (e.g.) a data frame of inputs. They also now return a specially
  classed object with a custom print method
  ([\#445](https://github.com/tidyverse/ellmer/issues/445)). New
  [`interpolate_package()`](https://ellmer.tidyverse.org/dev/reference/interpolate.md)
  makes it easier to interpolate from prompts stored in the
  `inst/prompts` directory inside a package
  ([\#164](https://github.com/tidyverse/ellmer/issues/164)).

- [`chat_anthropic()`](https://ellmer.tidyverse.org/dev/reference/chat_anthropic.md),
  `chat_azure()`,
  [`chat_openai()`](https://ellmer.tidyverse.org/dev/reference/chat_openai.md),
  and `chat_gemini()` now take a `params` argument, that coupled with
  the [`params()`](https://ellmer.tidyverse.org/dev/reference/params.md)
  helper, makes it easy to specify common model parameters (like `seed`
  and `temperature`) across providers. Support for other providers will
  grow as you request it
  ([\#280](https://github.com/tidyverse/ellmer/issues/280)).

- ellmer now tracks the cost of input and output tokens. The cost is
  displayed when you print a `Chat` object, in `tokens_usage()`, and
  with `Chat$get_cost()`. You can also request costs in
  [`parallel_chat_structured()`](https://ellmer.tidyverse.org/dev/reference/parallel_chat.md).
  We do our best to accurately compute the cost, but you should treat it
  as an estimate rather than the exact price. Unfortunately LLM
  providers currently make it very difficult to figure out exactly how
  much your queries cost
  ([\#203](https://github.com/tidyverse/ellmer/issues/203)).

### Provider updates

- We have support for three new providers:

  - [`chat_huggingface()`](https://ellmer.tidyverse.org/dev/reference/chat_huggingface.md)
    for models hosted at <https://huggingface.co>
    ([\#359](https://github.com/tidyverse/ellmer/issues/359),
    [@s-spavound](https://github.com/s-spavound)).
  - [`chat_mistral()`](https://ellmer.tidyverse.org/dev/reference/chat_mistral.md)
    for models hosted at <https://mistral.ai>
    ([\#319](https://github.com/tidyverse/ellmer/issues/319)).
  - [`chat_portkey()`](https://ellmer.tidyverse.org/dev/reference/chat_portkey.md)
    and
    [`models_portkey()`](https://ellmer.tidyverse.org/dev/reference/chat_portkey.md)
    for models hosted at <https://portkey.ai>
    ([\#363](https://github.com/tidyverse/ellmer/issues/363),
    [@maciekbanas](https://github.com/maciekbanas)).

- We also renamed (with deprecation) a few functions to make the naming
  scheme more consistent
  ([\#382](https://github.com/tidyverse/ellmer/issues/382),
  [@gadenbuie](https://github.com/gadenbuie)):

  - [`chat_azure_openai()`](https://ellmer.tidyverse.org/dev/reference/chat_azure_openai.md)
    replaces `chat_azure()`.
  - [`chat_aws_bedrock()`](https://ellmer.tidyverse.org/dev/reference/chat_aws_bedrock.md)
    replaces `chat_bedrock()`.
  - [`chat_anthropic()`](https://ellmer.tidyverse.org/dev/reference/chat_anthropic.md)
    replaces
    [`chat_anthropic()`](https://ellmer.tidyverse.org/dev/reference/chat_anthropic.md).
  - [`chat_google_gemini()`](https://ellmer.tidyverse.org/dev/reference/chat_google_gemini.md)
    replaces `chat_gemini()`.

- We have updated the default model for a couple of providers:

  - [`chat_anthropic()`](https://ellmer.tidyverse.org/dev/reference/chat_anthropic.md)
    uses Sonnet 3.7 (which it also now displays)
    ([\#336](https://github.com/tidyverse/ellmer/issues/336)).
  - [`chat_openai()`](https://ellmer.tidyverse.org/dev/reference/chat_openai.md)
    uses GPT-4.1
    ([\#512](https://github.com/tidyverse/ellmer/issues/512))

### Developer tooling

- New `Chat$get_provider()` lets you access the underlying provider
  object ([\#202](https://github.com/tidyverse/ellmer/issues/202)).

- `Chat$chat_async()` and `Chat$stream_async()` gain a `tool_mode`
  argument to decide between `"sequential"` and `"concurrent"` tool
  calling. This is an advanced feature that primarily affects
  asynchronous tools
  ([\#488](https://github.com/tidyverse/ellmer/issues/488),
  [@gadenbuie](https://github.com/gadenbuie)).

- `Chat$stream()` and `Chat$stream_async()` gain support for streaming
  the additional content types generated during a tool call with a new
  `stream` argument. When `stream = "content"` is set, the streaming
  response yields `Content` objects, including the `ContentToolRequest`
  and `ContentToolResult` objects used to request and return tool calls
  ([\#400](https://github.com/tidyverse/ellmer/issues/400),
  [@gadenbuie](https://github.com/gadenbuie)).

- New `Chat$on_tool_request()` and `$on_tool_result()` methods allow you
  to register callbacks to run on a tool request or tool result. These
  callbacks can be used to implement custom logging or other actions
  when tools are called, without modifying the tool function
  ([\#493](https://github.com/tidyverse/ellmer/issues/493),
  [@gadenbuie](https://github.com/gadenbuie)).

- `Chat$chat(echo = "output")` replaces the now-deprecated
  `echo = "text"` option. When using `echo = "output"`, additional
  output, such as tool requests and results, are shown as they occur.
  When `echo = "none"`, tool call failures are emitted as warnings
  ([\#366](https://github.com/tidyverse/ellmer/issues/366),
  [@gadenbuie](https://github.com/gadenbuie)).

- `ContentToolResult` objects can now be returned directly from the
  [`tool()`](https://ellmer.tidyverse.org/dev/reference/tool.md)
  function and now includes additional information
  ([\#398](https://github.com/tidyverse/ellmer/issues/398)
  [\#399](https://github.com/tidyverse/ellmer/issues/399),
  [@gadenbuie](https://github.com/gadenbuie)):

  - `extra`: A list of additional data associated with the tool result
    that is not shown to the chatbot.
  - `request`: The `ContentToolRequest` that triggered the tool call.
    `ContentToolResult` no longer has an `id` property, instead the tool
    call ID can be retrieved from `request@id`.

  They also include the error condition in the `error` property when a
  tool call fails
  ([\#421](https://github.com/tidyverse/ellmer/issues/421),
  [@gadenbuie](https://github.com/gadenbuie)).

- `ContentToolRequest` gains a `tool` property that includes the
  [`tool()`](https://ellmer.tidyverse.org/dev/reference/tool.md)
  definition when a request is matched to a tool by ellmer
  ([\#423](https://github.com/tidyverse/ellmer/issues/423),
  [@gadenbuie](https://github.com/gadenbuie)).

- [`tool()`](https://ellmer.tidyverse.org/dev/reference/tool.md) gains
  an `.annotations` argument that can be created with the
  [`tool_annotations()`](https://ellmer.tidyverse.org/dev/reference/tool_annotations.md)
  helper. Tool annotations are described in the [Model Context
  Protocol](https://modelcontextprotocol.io/introduction) and can be
  used to describe the tool to clients.
  ([\#402](https://github.com/tidyverse/ellmer/issues/402),
  [@gadenbuie](https://github.com/gadenbuie))

- New
  [`tool_reject()`](https://ellmer.tidyverse.org/dev/reference/tool_reject.md)
  function can be used to reject a tool request with an explanation for
  the rejection reason.
  [`tool_reject()`](https://ellmer.tidyverse.org/dev/reference/tool_reject.md)
  can be called within a tool function or in a `Chat$on_tool_request()`
  callback. In the latter case, rejecting a tool call will ensure that
  the tool function is not evaluated
  ([\#490](https://github.com/tidyverse/ellmer/issues/490),
  [\#493](https://github.com/tidyverse/ellmer/issues/493),
  [@gadenbuie](https://github.com/gadenbuie)).

### Minor improvements and bug fixes

- All requests now set a custom User-Agent that identifies that the
  requests come from ellmer
  ([\#341](https://github.com/tidyverse/ellmer/issues/341)). The default
  timeout has been increased to 5 minutes
  ([\#451](https://github.com/tidyverse/ellmer/issues/451),
  [\#321](https://github.com/tidyverse/ellmer/issues/321)).

- [`chat_anthropic()`](https://ellmer.tidyverse.org/dev/reference/chat_anthropic.md)
  now supports the thinking content type
  ([\#396](https://github.com/tidyverse/ellmer/issues/396)), and
  [`content_image_url()`](https://ellmer.tidyverse.org/dev/reference/content_image_url.md)
  ([\#347](https://github.com/tidyverse/ellmer/issues/347)). It gains a
  `beta_header` argument to opt-in to beta features
  ([\#339](https://github.com/tidyverse/ellmer/issues/339)). It (along
  with `chat_bedrock()`) no longer chokes after receiving an output that
  consists only of whitespace
  ([\#376](https://github.com/tidyverse/ellmer/issues/376)). Finally,
  `chat_anthropic(max_tokens =)` is now deprecated in favour of
  `chat_anthropic(params = )`
  ([\#280](https://github.com/tidyverse/ellmer/issues/280)).

- [`chat_google_gemini()`](https://ellmer.tidyverse.org/dev/reference/chat_google_gemini.md)
  and
  [`chat_google_vertex()`](https://ellmer.tidyverse.org/dev/reference/chat_google_gemini.md)
  gain more ways to authenticate. They can use `GEMINI_API_KEY` if set
  ([@t-kalinowski](https://github.com/t-kalinowski),
  [\#513](https://github.com/tidyverse/ellmer/issues/513)), authenticate
  with Google default application credentials (including service
  accounts, etc)
  ([\#317](https://github.com/tidyverse/ellmer/issues/317),
  [@atheriel](https://github.com/atheriel)) and use viewer-based
  credentials when running on Posit Connect
  ([\#320](https://github.com/tidyverse/ellmer/issues/320),
  [@atheriel](https://github.com/atheriel)). Authentication with default
  application credentials requires the {gargle} package. They now also
  can now handle responses that include citation metadata
  ([\#358](https://github.com/tidyverse/ellmer/issues/358)).

- [`chat_ollama()`](https://ellmer.tidyverse.org/dev/reference/chat_ollama.md)
  now works with
  [`tool()`](https://ellmer.tidyverse.org/dev/reference/tool.md)
  definitions with optional arguments or empty properties
  ([\#342](https://github.com/tidyverse/ellmer/issues/342),
  [\#348](https://github.com/tidyverse/ellmer/issues/348),
  [@gadenbuie](https://github.com/gadenbuie)), and now accepts `api_key`
  and consults the `OLLAMA_API_KEY` environment variable. This is not
  needed for local usage, but enables bearer-token authentication when
  Ollama is running behind a reverse proxy
  ([\#501](https://github.com/tidyverse/ellmer/issues/501),
  [@gadenbuie](https://github.com/gadenbuie)).

- `chat_openai(seed =)` is now deprecated in favour of
  `chat_openai(params = )`
  ([\#280](https://github.com/tidyverse/ellmer/issues/280)).

- [`create_tool_def()`](https://ellmer.tidyverse.org/dev/reference/create_tool_def.md)
  can now use any Chat instance
  ([\#118](https://github.com/tidyverse/ellmer/issues/118),
  [@pedrobtz](https://github.com/pedrobtz)).

- [`live_browser()`](https://ellmer.tidyverse.org/dev/reference/live_console.md)
  now requires {shinychat} v0.2.0 or later which provides access to the
  app that powers
  [`live_browser()`](https://ellmer.tidyverse.org/dev/reference/live_console.md)
  via
  [`shinychat::chat_app()`](https://posit-dev.github.io/shinychat/r/reference/chat_app.html),
  as well as a Shiny module for easily including a chat interface for an
  ellmer `Chat` object in your Shiny apps
  ([\#397](https://github.com/tidyverse/ellmer/issues/397),
  [@gadenbuie](https://github.com/gadenbuie)). It now initializes the UI
  with the messages from the chat turns, rather than replaying the turns
  server-side ([\#381](https://github.com/tidyverse/ellmer/issues/381)).

- `Provider` gains `name` and `model` fields
  ([\#406](https://github.com/tidyverse/ellmer/issues/406)). These are
  now reported when you print a chat object and are used in
  [`token_usage()`](https://ellmer.tidyverse.org/dev/reference/token_usage.md).

## ellmer 0.1.1

CRAN release: 2025-02-06

### Lifecycle changes

- `option(ellmer_verbosity)` is no longer supported; instead use the
  standard httr2 verbosity functions, such as
  [`httr2::with_verbosity()`](https://httr2.r-lib.org/reference/with_verbosity.html);
  these now support streaming data.

- `chat_cortex()` has been renamed `chat_cortex_analyst()` to better
  disambiguate it from
  [`chat_snowflake()`](https://ellmer.tidyverse.org/dev/reference/chat_snowflake.md)
  (which *also* uses “Cortex”)
  ([\#275](https://github.com/tidyverse/ellmer/issues/275),
  [@atheriel](https://github.com/atheriel)).

### New features

- All providers now wait for up to 60s to get the complete response. You
  can increase this with, e.g., `option(ellmer_timeout_s = 120)`
  ([\#213](https://github.com/tidyverse/ellmer/issues/213),
  [\#300](https://github.com/tidyverse/ellmer/issues/300)).

- `chat_azure()`,
  [`chat_databricks()`](https://ellmer.tidyverse.org/dev/reference/chat_databricks.md),
  [`chat_snowflake()`](https://ellmer.tidyverse.org/dev/reference/chat_snowflake.md),
  and `chat_cortex_analyst()` now detect viewer-based credentials when
  running on Posit Connect
  ([\#285](https://github.com/tidyverse/ellmer/issues/285),
  [@atheriel](https://github.com/atheriel)).

- [`chat_deepseek()`](https://ellmer.tidyverse.org/dev/reference/chat_deepseek.md)
  provides support for DeepSeek models
  ([\#242](https://github.com/tidyverse/ellmer/issues/242)).

- [`chat_openrouter()`](https://ellmer.tidyverse.org/dev/reference/chat_openrouter.md)
  provides support for models hosted by OpenRouter
  ([\#212](https://github.com/tidyverse/ellmer/issues/212)).

- [`chat_snowflake()`](https://ellmer.tidyverse.org/dev/reference/chat_snowflake.md)
  allows chatting with models hosted through Snowflake’s [Cortex LLM
  REST
  API](https://docs.snowflake.com/en/user-guide/snowflake-cortex/cortex-llm-rest-api)
  ([\#258](https://github.com/tidyverse/ellmer/issues/258),
  [@atheriel](https://github.com/atheriel)).

- [`content_pdf_file()`](https://ellmer.tidyverse.org/dev/reference/content_pdf_file.md)
  and
  [`content_pdf_url()`](https://ellmer.tidyverse.org/dev/reference/content_pdf_file.md)
  allow you to upload PDFs to supported models. Models that currently
  support PDFs are Google Gemini and Claude Anthropic. With help from
  [@walkerke](https://github.com/walkerke) and
  [@andrie](https://github.com/andrie)
  ([\#265](https://github.com/tidyverse/ellmer/issues/265)).

### Bug fixes and minor improvements

- `Chat$get_model()` returns the model name
  ([\#299](https://github.com/tidyverse/ellmer/issues/299)).

- `chat_azure()` has greatly improved support for Azure Entra ID. API
  keys are now optional and we can pick up on ambient credentials from
  Azure service principals or attempt to use interactive Entra ID
  authentication when possible. The broken-by-design `token` argument
  has been deprecated (it could not handle refreshing tokens properly),
  but a new `credentials` argument can be used for custom Entra ID
  support when needed instead (for instance, if you’re trying to use
  tokens generated by the `AzureAuth` package)
  ([\#248](https://github.com/tidyverse/ellmer/issues/248),
  [\#263](https://github.com/tidyverse/ellmer/issues/263),
  [\#273](https://github.com/tidyverse/ellmer/issues/273),
  [\#257](https://github.com/tidyverse/ellmer/issues/257),
  [@atheriel](https://github.com/atheriel)).

- `chat_azure()` now reports better error messages when the underlying
  HTTP requests fail
  ([\#269](https://github.com/tidyverse/ellmer/issues/269),
  [@atheriel](https://github.com/atheriel)). It now also defaults to
  `api_version = "2024-10-21"` which includes data for structured data
  extraction ([\#271](https://github.com/tidyverse/ellmer/issues/271)).

- `chat_bedrock()` now handles temporary IAM credentials better
  ([\#261](https://github.com/tidyverse/ellmer/issues/261),
  [@atheriel](https://github.com/atheriel)) and `chat_bedrock()` gains
  `api_args` argument ([@billsanto](https://github.com/billsanto),
  [\#295](https://github.com/tidyverse/ellmer/issues/295)).

- [`chat_databricks()`](https://ellmer.tidyverse.org/dev/reference/chat_databricks.md)
  now handles the `DATABRICKS_HOST` environment variable correctly
  whether it includes an HTTPS prefix or not
  ([\#252](https://github.com/tidyverse/ellmer/issues/252),
  [@atheriel](https://github.com/atheriel)). It also respects the
  `SPARK_CONNECT_USER_AGENT` environment variable when making requests
  ([\#254](https://github.com/tidyverse/ellmer/issues/254),
  [@atheriel](https://github.com/atheriel)).

- `chat_gemini()` now defaults to using the gemini-2.0-flash model.

- `print(Chat)` no longer wraps long lines, making it easier to read
  code and bulleted lists
  ([\#246](https://github.com/tidyverse/ellmer/issues/246)).

## ellmer 0.1.0

CRAN release: 2025-01-09

- New
  [`chat_vllm()`](https://ellmer.tidyverse.org/dev/reference/chat_vllm.md)
  to chat with models served by vLLM
  ([\#140](https://github.com/tidyverse/ellmer/issues/140)).

- The default
  [`chat_openai()`](https://ellmer.tidyverse.org/dev/reference/chat_openai.md)
  model is now GPT-4o.

- New `Chat$set_turns()` to set turns. `Chat$turns()` is now
  `Chat$get_turns()`. `Chat$system_prompt()` is replaced with
  `Chat$set_system_prompt()` and `Chat$get_system_prompt()`.

- Async and streaming async chat are now event-driven and use
  [`later::later_fd()`](https://later.r-lib.org/reference/later_fd.html)
  to wait efficiently on curl socket activity
  ([\#157](https://github.com/tidyverse/ellmer/issues/157)).

- New `chat_bedrock()` to chat with AWS bedrock models
  ([\#50](https://github.com/tidyverse/ellmer/issues/50)).

- New `chat$extract_data()` uses the structured data API where available
  (and tool calling otherwise) to extract data structured according to a
  known type specification. You can create specs with functions
  [`type_boolean()`](https://ellmer.tidyverse.org/dev/reference/type_boolean.md),
  [`type_integer()`](https://ellmer.tidyverse.org/dev/reference/type_boolean.md),
  [`type_number()`](https://ellmer.tidyverse.org/dev/reference/type_boolean.md),
  [`type_string()`](https://ellmer.tidyverse.org/dev/reference/type_boolean.md),
  [`type_enum()`](https://ellmer.tidyverse.org/dev/reference/type_boolean.md),
  [`type_array()`](https://ellmer.tidyverse.org/dev/reference/type_boolean.md),
  and
  [`type_object()`](https://ellmer.tidyverse.org/dev/reference/type_boolean.md)
  ([\#31](https://github.com/tidyverse/ellmer/issues/31)).

- The general `ToolArg()` has been replaced by the more specific
  `type_*()` functions.
  [`ToolDef()`](https://ellmer.tidyverse.org/dev/reference/tool.md) has
  been renamed to `tool`.

- [`content_image_url()`](https://ellmer.tidyverse.org/dev/reference/content_image_url.md)
  will now create inline images when given a data url
  ([\#110](https://github.com/tidyverse/ellmer/issues/110)).

- Streaming ollama results works once again
  ([\#117](https://github.com/tidyverse/ellmer/issues/117)).

- Streaming OpenAI results now capture more results, including
  `logprobs` ([\#115](https://github.com/tidyverse/ellmer/issues/115)).

- New
  [`interpolate()`](https://ellmer.tidyverse.org/dev/reference/interpolate.md)
  and `prompt_file()` make it easier to create prompts that are a mix of
  static text and dynamic values.

- You can find how many tokens you’ve used in the current session by
  calling
  [`token_usage()`](https://ellmer.tidyverse.org/dev/reference/token_usage.md).

- `chat_browser()` and `chat_console()` are now
  [`live_browser()`](https://ellmer.tidyverse.org/dev/reference/live_console.md)
  and
  [`live_console()`](https://ellmer.tidyverse.org/dev/reference/live_console.md).

- The `echo` can now be one of three values: “none”, “text”, or “all”.
  If “all”, you’ll now see both user and assistant turns, and all
  content types will be printed, not just text. When running in the
  global environment, `echo` defaults to “text”, and when running inside
  a function it defaults to “none”.

- You can now log low-level JSON request/response info by setting
  `options(ellmer_verbosity = 2)`.

- `chat$register_tool()` now takes an object created by `Tool()`. This
  makes it a little easier to reuse tool definitions
  ([\#32](https://github.com/tidyverse/ellmer/issues/32)).

- `new_chat_openai()` is now
  [`chat_openai()`](https://ellmer.tidyverse.org/dev/reference/chat_openai.md).

- Claude and Gemini are now supported via
  [`chat_claude()`](https://ellmer.tidyverse.org/dev/reference/chat_anthropic.md)
  and `chat_gemini()`.

- The Snowflake Cortex Analyst is now supported via `chat_cortex()`
  ([\#56](https://github.com/tidyverse/ellmer/issues/56)).

- Databricks is now supported via
  [`chat_databricks()`](https://ellmer.tidyverse.org/dev/reference/chat_databricks.md)
  ([\#152](https://github.com/tidyverse/ellmer/issues/152)).
