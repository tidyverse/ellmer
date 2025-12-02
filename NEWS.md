# ellmer (development version)

* `chat_github()` now uses `chat_openai_compatible()` for improved compatibility, and `models_github()` now supports custom `base_url` configuration (@D-M4rk, #877).
* `chat_ollama()` now contains a slot for `top_k` within the `params` argument (@frankiethull).

# ellmer 0.4.0

## Lifecycle changes

* `chat_claude()` is no longer deprecated and is an alias for `chat_anthropic()`, reflecting Anthropic's recent rebranding of developer tools under the Claude name (#758). `models_claude()` is now an alias for `models_anthropic()`.
* `parallel_chat()` and `batch_chat()` are no longer experimental.
* The following deprecated functions/arguments/methods have now been removed:
  * `Chat$extract_data()` -> `chat$chat_structured()` (0.2.0)
  * `Chat$extract_data_async()` -> `chat$chat_structured_async()` (0.2.0)
  * `chat_anthropic(max_tokens)` -> `chat_anthropic(params)` (0.2.0)
  * `chat_azure()` -> `chat_azure_openai()` (0.2.0)
  * `chat_azure_openai(token)` (0.1.1)
  * `chat_bedrock()` -> `chat_aws_bedrock()` (0.2.0)
  * `chat_claude()` -> `chat_anthropic()` (0.2.0)
  * `chat_cortex()` -> `chat_snowflake()` (0.2.0)
  * `chat_gemini()` -> `chat_google_gemini()` (0.2.0)
  * `chat_openai(seed)` -> `chat_openai(params)` (0.2.0)
  * `create_tool_def(model)` -> `create_tool_def(chat)` (0.2.0)

## New features

* `batch_*()` no longer hashes properties of the provider besides the `name`, `model`, and `base_url`. This should provide some protection from accidentally reusing the same `.json` file with different providers, while still allowing you to use the same batch file across ellmer versions. It also has a new `ignore_hash` argument that allows you to opt out of the check if you're confident the difference only arises because ellmer itself has changed.
* `chat_claude()` gains new `cache` parameter to control caching. By default it is set to "5m". This should (on average) reduce the cost of your chats (#584).
* `chat_openai()` now uses OpenAI's responses endpoint (#365, #801). This is their recommended endpoint and gives more access to built-in tools.
* `chat_openai_compatible()` replaces `chat_openai()` as the interface to use for OpenAI-compatible APIs, and `chat_openai()` is reserved for the official OpenAI API. Unlike previous versions of `chat_openai()`, the `base_url` parameter is now required (#801).
* `chat_*()` functions now use a `credentials` function instead of an `api_key` (#613). This means that API keys are never stored in the chat object (which might be saved to disk), but are instead retrieved on demand as needed. You generally shouldn't need to use the `credentials` argument, but when you do, you should use it to dynamically retrieve the API key from some other source (i.e. never inline a secret directly into a function call).
* New set of `claude_file_()` functions for managing file uploads with Claude (@dcomputing, #761).
* ellmer now supports a variety of built-in web search and fetch tools (#578):
  - `claude_tool_web_search()` and `claude_tool_web_fetch()` for Claude.
  - `google_tool_web_search()` and `google_tool_web_fetch()` for Gemini.
  - `openai_tool_web_search()` for OpenAI.
  If you want to do web fetch for other providers, you could use `btw::btw_tool_web_read_url()`.
* `parallel_chat()` and friends now have a more permissive attitude to errors. By default, they will now return when hitting the first error (rather than erroring), and you can control this behaviour with the `on_error` argument. Or if you interrupt the job, it will finish up current requests and then return all the work done so far. The main downside of this work is that the output of `parallel_chat()` is more complex: it is now a mix of `Chat` objects, error objects, and `NULL` (#628).
* `parallel_chat_structured()` no longer errors if some results fail to parse. Instead it warns, and the corresponding rows will be filled in with the appropriate missing values (#628).
* New `schema_df()` to describe the schema of a data frame to an LLM (#744).
* `tool()`s can now return image or PDF content types, with `content_image_file()` or `content_image_pdf()` (#735).
* `params()` gains new `reasoning_effort` and `reasoning_tokens` so you can control the amount of effort a model spends on thinking. Initial support is provided for `chat_claude()`, `chat_google_gemini()`, and `chat_openai()` (#720).
* New `type_ignore()` allows you to specify that a tool argument should not be provided by the LLM when the R function has a suitable default value (#764).

## Minor improvements and bug fixes

* Updated pricing data (#790).
* `AssistantTurn`s now have a `@duration` slot, containing the total time to complete the request (@simonpcouch, #798).
* `batch_chat()` logs tokens once, on retrieval (#743).
* `batch_chat()` now retrieves failed results for `chat_openai()` (#830) and gracefully handles invalid JSON (#845).
* `batch_chat()` now works once more for `chat_anthropic()` (#835).
* `batch_chat_*()` and `parallel_chat_*()` now accept a string as the chat object, following the same rules as `chat()` (#677).
* `chat_claude()` and `chat_aws_bedrock()` now default to Claude Sonnet 4.5 (#800).
* `chat_databricks()` lifts many of its restrictions now that Databricks' API is more OpenAI compatible (#757).
* `chat_google_gemini()` and `chat_openai()` support image generation (#368).
* `chat_google_gemini()` has an experimental fallback interactive OAuth flow, if you're in an interactive session and no other authentication options can be found (#680).
* `chat_groq()` now defaults to llama-3.1-8b-instant.
* `chat_openai()` gains a `service_tier` argument (#712).
* `chat_portkey()` now requires you to supply a model (#786).
* `chat_portkey(virtual_key)` no longer needs to be supplied; instead Portkey recommends including the virtual key/provider in the `model` (#786).
* `Chat$chat()`, `Chat$stream()`, and similar methods now add empty tool results when a the chat is interrupted during a tool call loop, allowing the conversation to be resumed without causing an API error (#840).
* `Chat$chat_structured()` and friends now only warn if multiple JSON payloads found (instead of erroring) (@kbenoit, #732).
* `Chat$get_tokens()` gives a brief description of the turn contents to make it easier to see which turn tokens are spent on (#618) and also returns the cost (#824). It now returns one row for each assistant turn, better representing the underlying data received from LLM APIs. Similarly, the `print()` method now reports costs on each assistant turn, rather than trying to parse out individual costs.
* `interpolate_package()` now provides an informative error if the requested prompt file is not found in the package's `prompts/` directory (#763) and now works with in-development packages loaded with devtools (#766).
* `models_mistral()` lists available models (@rplsmn, #750).
* `models_ollama()` was fixed to correctly query model capabilities from remote Ollama servers (#746).
* `chat_ollama()` now uses `credentials` when checking if Ollama is available and `models_ollama()` now has a `credentials` argument. This is useful when accessing Ollama servers that require authentication (@AdaemmerP, #863).
* `parallel_chat_structured()` now returns a tibble, since this does a better job of printing more complex data frames (#787).

# ellmer 0.3.2

* `chat()` is now compatible with most `chat_` functions (#699).
  * `chat_aws_bedrock()`, `chat_databricks()`, `chat_deepseek()`, `chat_github()`, `chat_groq()`, `chat_ollama()`, `chat_openrouter()`, `chat_perplexity()`, and `chat_vllm()` now support a `params` argument that accepts common model parameters from `params()`.
  * The `deployment_id` argument in `chat_azure_openai()` was deprecated and replaced with `model` to better align with other providers.

* `chat_openai()` now correctly maps `max_tokens` and `top_k` from `params()` to the OpenAI API parameters (#699).

# ellmer 0.3.1

* `chat_anthropic()` drops empty assistant turns to avoid API errors (#710).

* `chat_github()` now uses the `https://models.github.ai/inference` endpoint and `chat()` supports GitHub models in the format `chat("github/openai/gpt-4.1")` (#726).

* `chat_google_vertex()` authentication was fixed using broader scope (#704, @netique)

* `chat_google_vertex()` can now use `global` project location (#704, @netique)

* `chat_openai()` now uses `OPENAI_BASE_URL`, if set, for the `base_url`. Similarly, `chat_ollama()` also uses `OLLAMA_BASE_URL` if set (#713).

* `contents_record()` and `contents_replay()` now record and replay custom classes that extend ellmer's `Turn` or `Content` classes (#689). `contents_replay()` now also restores the tool definition in `ContentToolResult` objects (in `@request@tool`) (#693).

* `chat_snowflake()` now supports Privatelink accounts (#694, @robert-norberg). and works against Snowflake's latest API changes (#692, @robert-norberg).

* `models_google_vertex()` works once again (#704, @netique)

* In the `value_turn()` method for OpenAI providers, `usage` is checked if `NULL` before logging tokens to avoid errors when streaming with some OpenAI-compatible services (#706, @stevegbrooks).

# ellmer 0.3.0

## New features

* New `chat()` allows you to chat with any provider using a string like `chat("anthropic")` or `chat("openai/gpt-4.1-nano")` (#361).

* `tool()` has a simpler specification: you now specify the `name`, `description`, and `arguments`. I have done my best to deprecate old usage and give clear errors, but I have likely missed a few edge cases. I apologize for any pain that this causes, but I'm convinced that it is going to make tool usage easier and clearer in the long run. If you have many calls to convert, `?tool` contains a prompt that will help you use an LLM to convert them (#603). It also now returns a function so that you can call it (and/or export it from your package) (#602).

* `type_array()` and `type_enum()` now have the description as the second argument and `items`/`values` as the first. This makes them easier to use in the common case where the description isn't necessary (#610).

* ellmer now retries requests up to 3 times, controllable with `option(ellmer_max_tries)`, and will retry if the connection fails (rather than just if the request itself returns a transient error). The default timeout, controlled by `option(ellmer_timeout_s)`, now applies to the initial connection phase. Together, these changes should make it much more likely for ellmer requests to succeed.

* New `parallel_chat_text()` and `batch_chat_text()` make it easier to just get the text response from multiple prompts (#510).

* ellmer's cost estimates are considerably improved. `chat_openai()`, `chat_google_gemini()`, and `chat_anthropic()` capture the number of cached input tokens. This is primarily useful for OpenAI and Gemini since both offer automatic caching, yielding improved cost estimates (#466). We also have a better source of pricing data, LiteLLM. This considerably expands the number of providers and models that include cost information (#659).

## Bug fixes and minor improvements

* The new `ellmer_echo` option controls the default value for `echo`.
* `batch_chat_structured()` provides clear messaging when prompts/path/provider don't match (#599).
* `chat_aws_bedrock()` allows you to set the `base_url()` (#441).
* `chat_aws_bedrock()`, `chat_google_gemini()`, `chat_ollama()`, and `chat_vllm()` use a more robust method to generate model URLs from the `base_url` (#593, @benyake).
* `chat_cortex_analyst()` is deprecated; please use `chat_snowflake()` instead (#640).
* `chat_github()` (and other OpenAI extensions) no longer warn about `seed` (#574).
* `chat_google_gemini()` and `chat_google_vertex()` default to Gemini 2.5 flash (#576).
* `chat_huggingface()` works much better.
* `chat_openai()` supports `content_pdf_()` (#650).
* `chat_portkey()` works once again, and reads the virtual API key from the `PORTKEY_VIRTUAL_KEY` env var (#588).
* `chat_snowflake()` works with tool calling (#557, @atheriel).
* `Chat$chat_structured()` and friends no longer unnecessarily wrap `type_object()` for `chat_openai()` (#671).
* `Chat$chat_structured()` suppresses tool use. If you need to use tools and structured data together, first use `$chat()` for any needed tools, then `$chat_structured()` to extract the data you need.
* `Chat$chat_structured()` no longer requires a prompt (since it may be obvious from the context) (#570).
* `Chat$register_tool()` shows a message when you replace an existing tool (#625).
* `contents_record()` and `contents_replay()` record and replay `Turn` related information from a `Chat` instance (#502). These methods can be used for bookmarking within {shinychat}.
* `models_github()` lists models for `chat_github()` (#561).
* `models_ollama()` includes a `capabilities` column with a comma-separated list of model capabilities (#623).
* `parallel_chat()` and friends accept lists of `Content` objects in the `prompt` (#597, @thisisnic).
* Tool requests show converted arguments when printed (#517).
* `tool()` checks that the `name` is valid (#625).

# ellmer 0.2.1

* When you save a `Chat` object to disk, API keys are
  This means that you can no longer easily resume a chat you've saved on disk
  (we'll figure this out in a future release) but ensures that you never
  accidentally save your secret key in an RDS file (#534).

* `chat_anthropic()` now defaults to Claude Sonnet 4, and I've added pricing
  information for the latest generation of Claude models.

* `chat_databricks()` now picks up on Databricks workspace URLs set in the
  configuration file, which should improve compatibility with the Databricks CLI
  (#521, @atheriel). It now also supports tool calling (#548, @atheriel).

* `chat_snowflake()` no longer streams answers that include a mysterious
  `list(type = "text", text = "")` trailer (#533, @atheriel). It now parses
  streaming outputs correctly into turns (#542), supports structured ouputs
  (#544), and standard model parameters (#545, @atheriel).

* `chat_snowflake()` and `chat_databricks()` now default to Claude Sonnet 3.7,
  the same default as `chat_anthropic()` (#539 and #546, @atheriel).

* `type_from_schema()` lets you to use pre-existing JSON schemas in structured
  chats (#133, @hafen)

# ellmer 0.2.0

## Breaking changes

* We have made a number of refinements to the way ellmer converts JSON
  to R data structures. These are breaking changes, although we don't expect
  them to affect much code in the wild. Most importantly, tools are now invoked
  with their inputs coerced to standard R data structures (#461); opt-out
  by setting `convert = FALSE` in `tool()`.

  Additionally ellmer now converts `NULL` to `NA` for `type_boolean()`,
  `type_integer()`, `type_number()`, and `type_string()` (#445), and does a
  better job with arrays when `required = FALSE` (#384).

* `chat_` functions no longer have a `turn` argument. If you need to set the
  turns, you can now use `Chat$set_turns()` (#427). Additionally,
  `Chat$tokens()` has been renamed to `Chat$get_tokens()` and returns a data
  frame of tokens, correctly aligned to the individual turn. The print method
  now uses this to show how many input/output tokens were used by each turn
  (#354).

## New features

* Two new interfaces help you do multiple chats with a single function call:

  * `batch_chat()` and `batch_chat_structured()` allow you to submit multiple
    chats to OpenAI and Anthropic's batched interfaces. These only guarantee a
    response within 24 hours, but are 50% of the price of regular requests
    (#143).

  * `parallel_chat()` and `parallel_chat_structured()` work with any provider
    and allow you to submit multiple chats in parallel (#143). This doesn't give
    you any cost savings, but it's can be much, much faster.

  This new family of functions is experimental because I'm not 100% sure that
  the shape of the user interface is correct, particularly as it pertains to
  handling errors.

* `google_upload()` lets you upload files to Google Gemini or Vertex AI (#310).
  This allows you to work with videos, PDFs, and other large files with Gemini.

* `models_google_gemini()`, `models_anthropic()`, `models_openai()`,
  `models_aws_bedrock()`, `models_ollama()` and `models_vllm()`, list available
  models for Google Gemini, Anthropic, OpenAI, AWS Bedrock, Ollama, and VLLM
  respectively. Different providers return different metadata so they are only
  guaranteed to return a data frame with at least an `id` column (#296).
  Where possible (currently for Gemini, Anthropic, and OpenAI) we include
  known token prices (per million tokens).

* `interpolate()` and friends are now vectorised so you can generate multiple
  prompts for (e.g.) a data frame of inputs. They also now return a specially
  classed object with a custom print method (#445). New `interpolate_package()`
  makes it easier to interpolate from prompts stored in the `inst/prompts`
  directory inside a package (#164).

* `chat_anthropic()`, `chat_azure()`, `chat_openai()`, and `chat_gemini()` now
  take a `params` argument, that coupled with the `params()` helper, makes it
  easy to specify common model parameters (like `seed` and `temperature`)
  across providers. Support for other providers will grow as you request it
  (#280).

* ellmer now tracks the cost of input and output tokens. The cost is displayed
  when you print a `Chat` object, in `tokens_usage()`, and with
  `Chat$get_cost()`. You can also request costs in `parallel_chat_structured()`.
  We do our best to accurately compute the cost, but you should treat it as an
  estimate rather than the exact price. Unfortunately LLM providers currently
  make it very difficult to figure out exactly how much your queries cost (#203).

## Provider updates

* We have support for three new providers:

  * `chat_huggingface()` for models hosted at <https://huggingface.co>
    (#359, @s-spavound).
  * `chat_mistral()` for models hosted at <https://mistral.ai> (#319).
  * `chat_portkey()` and `models_portkey()` for models hosted at
    <https://portkey.ai> (#363, @maciekbanas).

* We also renamed (with deprecation) a few functions to make the naming
  scheme more consistent (#382, @gadenbuie):

  * `chat_azure_openai()` replaces `chat_azure()`.
  * `chat_aws_bedrock()` replaces `chat_bedrock()`.
  * `chat_anthropic()` replaces `chat_anthropic()`.
  * `chat_google_gemini()` replaces `chat_gemini()`.

* We have updated the default model for a couple of providers:
  * `chat_anthropic()` uses Sonnet 3.7 (which it also now displays) (#336).
  * `chat_openai()` uses GPT-4.1 (#512)

## Developer tooling

* New `Chat$get_provider()` lets you access the underlying provider object
  (#202).

* `Chat$chat_async()` and `Chat$stream_async()` gain a `tool_mode` argument to
  decide between `"sequential"` and `"concurrent"` tool calling. This is an
  advanced feature that primarily affects asynchronous tools (#488, @gadenbuie).

* `Chat$stream()` and `Chat$stream_async()` gain support for streaming the
  additional content types generated during a tool call with a new `stream`
  argument. When `stream = "content"` is set, the streaming response yields
  `Content` objects, including the `ContentToolRequest` and `ContentToolResult`
  objects used to request and return tool calls (#400, @gadenbuie).

* New `Chat$on_tool_request()` and `$on_tool_result()` methods allow you to
  register callbacks to run on a tool request or tool result. These callbacks
  can be used to implement custom logging or other actions when tools are
  called, without modifying the tool function (#493, @gadenbuie).

* `Chat$chat(echo = "output")` replaces the now-deprecated `echo = "text"`
  option. When using `echo = "output"`, additional output, such as tool
  requests and results, are shown as they occur. When `echo = "none"`, tool
  call failures are emitted as warnings (#366, @gadenbuie).

* `ContentToolResult` objects can now be returned directly from the `tool()`
  function and now includes additional information (#398 #399, @gadenbuie):

  * `extra`: A list of additional data associated with the tool result that is
    not shown to the chatbot.
  * `request`: The `ContentToolRequest` that triggered the tool call.
    `ContentToolResult` no longer has an `id` property, instead the tool call
    ID can be retrieved from `request@id`.

  They also include the error condition in the `error` property when a tool call
  fails (#421, @gadenbuie).

* `ContentToolRequest` gains a `tool` property that includes the `tool()`
  definition when a request is matched to a tool by ellmer (#423, @gadenbuie).

* `tool()` gains an `.annotations` argument that can be created with the
  `tool_annotations()` helper. Tool annotations are described in the
  [Model Context Protocol](https://modelcontextprotocol.io/introduction) and can
  be used to describe the tool to clients. (#402, @gadenbuie)

* New `tool_reject()` function can be used to reject a tool request with an
  explanation for the rejection reason. `tool_reject()` can be called within a
  tool function or in a `Chat$on_tool_request()` callback. In the latter case,
  rejecting a tool call will ensure that the tool function is not evaluated
  (#490, #493, @gadenbuie).

## Minor improvements and bug fixes

* All requests now set a custom User-Agent that identifies that the requests
  come from ellmer (#341). The default timeout has been increased to
  5 minutes (#451, #321).

* `chat_anthropic()` now supports the thinking content type (#396), and
  `content_image_url()` (#347). It gains a `beta_header` argument to opt-in
  to beta features (#339). It (along with `chat_bedrock()`) no longer chokes
  after receiving an output that consists only of whitespace (#376).
  Finally, `chat_anthropic(max_tokens =)` is now deprecated in favour of
  `chat_anthropic(params = )` (#280).

* `chat_google_gemini()` and `chat_google_vertex()` gain more ways to
  authenticate. They can use `GEMINI_API_KEY` if set (@t-kalinowski, #513),
  authenticate with Google default application credentials (including service
  accounts, etc) (#317, @atheriel) and use viewer-based credentials when
  running on Posit Connect (#320, @atheriel). Authentication with default
  application credentials requires the {gargle} package. They now also can now
  handle responses that include citation metadata (#358).

* `chat_ollama()` now works with `tool()` definitions with optional arguments
  or empty properties (#342, #348, @gadenbuie), and now accepts `api_key` and
  consults the `OLLAMA_API_KEY` environment variable. This is not needed for
  local usage, but enables bearer-token authentication when Ollama is running
  behind a reverse proxy (#501, @gadenbuie).

* `chat_openai(seed =)` is now deprecated in favour of `chat_openai(params = )`
  (#280).

* `create_tool_def()` can now use any Chat instance (#118, @pedrobtz).

* `live_browser()` now requires {shinychat} v0.2.0 or later which provides
  access to the app that powers `live_browser()` via `shinychat::chat_app()`,
  as well as a Shiny module for easily including a chat interface for an ellmer
  `Chat` object in your Shiny apps (#397, @gadenbuie). It now initializes the
  UI with the messages from the chat turns, rather than replaying the turns
  server-side (#381).

* `Provider` gains `name` and `model` fields (#406). These are now reported when
  you print a chat object and are used in `token_usage()`.

# ellmer 0.1.1

## Lifecycle changes

* `option(ellmer_verbosity)` is no longer supported; instead use the standard
  httr2 verbosity functions, such as `httr2::with_verbosity()`; these now
  support streaming data.

* `chat_cortex()` has been renamed `chat_cortex_analyst()` to better
  disambiguate it from `chat_snowflake()` (which *also* uses "Cortex")
  (#275, @atheriel).

## New features

* All providers now wait for up to 60s to get the complete response. You can
  increase this with, e.g., `option(ellmer_timeout_s = 120)` (#213, #300).

* `chat_azure()`, `chat_databricks()`, `chat_snowflake()`, and
  `chat_cortex_analyst()` now detect viewer-based credentials when running on
  Posit Connect (#285, @atheriel).

* `chat_deepseek()` provides support for DeepSeek models (#242).

* `chat_openrouter()` provides support for models hosted by OpenRouter (#212).

* `chat_snowflake()` allows chatting with models hosted through Snowflake's
  [Cortex LLM REST API](https://docs.snowflake.com/en/user-guide/snowflake-cortex/cortex-llm-rest-api)
  (#258, @atheriel).

* `content_pdf_file()` and `content_pdf_url()` allow you to upload PDFs to
  supported models. Models that currently support PDFs are Google Gemini and
  Claude Anthropic. With help from @walkerke and @andrie (#265).

## Bug fixes and minor improvements

* `Chat$get_model()` returns the model name (#299).

* `chat_azure()` has greatly improved support for Azure Entra ID. API keys are
  now optional and we can pick up on ambient credentials from Azure service
  principals or attempt to use interactive Entra ID authentication when
  possible. The broken-by-design `token` argument has been deprecated (it
  could not handle refreshing tokens properly), but a new `credentials`
  argument can be used for custom Entra ID support when needed instead
  (for instance, if you're trying to use tokens generated by the `AzureAuth`
  package) (#248, #263, #273, #257, @atheriel).

* `chat_azure()` now reports better error messages when the underlying HTTP
  requests fail (#269, @atheriel). It now also defaults to
  `api_version = "2024-10-21"` which includes data for structured data
  extraction (#271).

* `chat_bedrock()` now handles temporary IAM credentials better
  (#261, @atheriel) and `chat_bedrock()` gains `api_args` argument (@billsanto, #295).

* `chat_databricks()` now handles the `DATABRICKS_HOST` environment variable
  correctly whether it includes an HTTPS prefix or not (#252, @atheriel).
  It also respects the `SPARK_CONNECT_USER_AGENT` environment variable when
  making requests (#254, @atheriel).

* `chat_gemini()` now defaults to using the gemini-2.0-flash model.

* `print(Chat)` no longer wraps long lines, making it easier to read code
  and bulleted lists (#246).

# ellmer 0.1.0

* New `chat_vllm()` to chat with models served by vLLM (#140).

* The default `chat_openai()` model is now GPT-4o.

* New `Chat$set_turns()` to set turns. `Chat$turns()` is now `Chat$get_turns()`. `Chat$system_prompt()` is replaced with `Chat$set_system_prompt()` and `Chat$get_system_prompt()`.

* Async and streaming async chat are now event-driven and use `later::later_fd()` to wait efficiently on curl socket activity (#157).

* New `chat_bedrock()` to chat with AWS bedrock models (#50).

* New `chat$extract_data()` uses the structured data API where available (and tool calling otherwise) to extract data structured according to a known type specification. You can create specs with functions `type_boolean()`, `type_integer()`, `type_number()`, `type_string()`, `type_enum()`, `type_array()`, and `type_object()` (#31).

* The general `ToolArg()` has been replaced by the more specific `type_*()` functions. `ToolDef()` has been renamed to `tool`.

* `content_image_url()` will now create inline images when given a data url (#110).

* Streaming ollama results works once again (#117).

* Streaming OpenAI results now capture more results, including `logprobs` (#115).

* New `interpolate()` and `prompt_file()` make it easier to create prompts that are a mix of static text and dynamic values.

* You can find how many tokens you've used in the current session by calling `token_usage()`.

* `chat_browser()` and `chat_console()` are now `live_browser()` and `live_console()`.

* The `echo` can now be one of three values: "none", "text", or "all". If "all", you'll now see both user and assistant turns, and all content types will be printed, not just text. When running in the global environment, `echo` defaults to "text", and when running inside a function it defaults to "none".

* You can now log low-level JSON request/response info by setting `options(ellmer_verbosity = 2)`.

* `chat$register_tool()` now takes an object created by `Tool()`. This makes it a little easier to reuse tool definitions (#32).

* `new_chat_openai()` is now `chat_openai()`.

* Claude and Gemini are now supported via `chat_claude()` and `chat_gemini()`.

* The Snowflake Cortex Analyst is now supported via `chat_cortex()` (#56).

* Databricks is now supported via `chat_databricks()` (#152).
