# ellmer (development version)

* `$chat_async()` and `$stream_async()` gain a `tool_mode` argument to decide
  between `"sequential"` and `"concurrent"` tool calling. This is an advanced
  feature that primarily affects asynchronous tools (#488, @gadenbuie).
  
* `models_google_gemini()`, `models_anthropic()`, `models_openai()`,  
  `models_aws_bedrock()`, `models_ollama()` and `models_vllm()`, list available 
  models for Google Gemini, Anthropic, OpenAI, AWS Bedrock, Ollama, and VLLM 
  respectively. Different providers return different metadata so they are only 
  guaranteed to return a data frame with at least an `id` column (#296).
  Where possible (currently for Gemini, Anthropic, and OpenAI) we include
  known token prices (per million tokens).

* Added a Shiny app example in `vignette("streaming-async")` showcasing
  asynchronous streaming with `{ellmer}` and `{shinychat}` (#131, @gadenbuie,
  @adisarid).

* New `chat_huggingface()` for models hosted at <https://huggingface.co>
  (#359, @s-spavound).

* Bumped default time out up to 5 minutes (#451, #321).

* BREAKING CHANGE: Tools are now invoked with their inputs coerced to standard
  R data structures (#461).

* `$extract_data(convert = TRUE)` now converts `NULL` to `NA` for
  `type_boolean()`, `type_integer()`, `type_number()`, and `type_string()`
  (#445).

* `interpolate()` and friends are now vectorised so you can generate multiple
  prompts for (e.g.) a data frame of inputs. They also now return a specially
  classed object with a custom print method (#445).

* `live_browser()` now requires `{shinychat}` v0.2.0 or later which provides
  access to the app that powers `live_browser()` via `shinychat::chat_app()`,
  as well as Shiny module for easily including a chat interface for an ellmer
  `Chat` object in your Shiny apps (#397, @gadenbuie).

* New `chat_mistral()` for models hosted at <https://mistral.ai> (#319).

* `chat_gemini()` can now handle responses that include citation metadata
  (#358).

* `chat_` functions no longer take a turns object, instead use `set_turns()`
  (#427).

* `echo = "output"` replaces the now-deprecated `echo = "text"` option in
  `Chat$chat()`. When using `echo = "output"`, additional output, such as tool
  requests and results, are shown as they occur. When `echo = "none"`, tool
  call failures are emitted as warnings (#366, @gadenbuie).

* `ContentToolResult` objects can now be returned directly from the `tool()`
  function and now includes additional information (#398 #399, @gadenbuie):

  * `extra`: A list of additional data associated with the tool result that is
    not shown to the chatbot.
  * `request`: The `ContentToolRequest` that triggered the tool call.
    `ContentToolResult` no longer has an `id` property, instead the tool call
    ID can be retrieved from `request@id`.

* `ContentToolRequest` gains a `tool` property that includes the `tool()`
  definition when a request is matched to a tool by ellmer (#423, @gadenbuie).

* ellmer now tracks the cost of input and output tokens. The cost is displayed
  when you print a `Chat` object, in `tokens_usage()`, and with
  `Chat$get_cost()`. You can also request costs in `$parallel_extract_data()`.

  We do our best to accurately compute the cost, but you should treat it as an
  estimate rather than the exact price. Unfortunately LLM APIs currently make it
  very hard to figure out exactly how much your queries cost (#203).

* `ContentToolResult` objects now include the error condition in the `error`
  property when a tool call fails (#421, @gadenbuie).

* Several chat functions were renamed to better align with the companies
  providing the API (#382, @gadenbuie):

  * `chat_azure_openai()` replaces `chat_azure()`
  * `chat_aws_bedrock()` replaces `chat_bedrock()`
  * `chat_anthropic()` replaces `chat_claude()`
  * `chat_google_gemini()` replaces `chat_gemini()`

* `chat_claude()` now supports the thinking content type (#396).

* `tool()` gains an `.annotations` argument that can be created with the
  `tool_annotations()` helper. Tool annotations are described in the
  [Model Context Protocol](https://modelcontextprotocol.io/introduction) and can
  be used to describe the tool to clients. (#402, @gadenbuie)

* `Provider` gains `name` and `model` fields (#406). These are now reported when
  you print a chat object and used in `token_usage()`.

* New `interpolate_package()` to make it easier to interpolate from prompts
  stored in the `inst/prompts` inside a package (#164).

* `chat_azure()`, `chat_claude()`, `chat_openai()`, and `chat_gemini()` now have
  a `params`  argument that allows you to specify common model paramaters (like
  `seed` and `temperature`). Support for other models will grow as you request
  it (#280).

* `chat_claude(max_tokens =)` is now deprecated in favour of
  `chat_claude(params = )` (#280).

* `chat_openai(seed =)` is now deprecated in favour of
  `chat_openai(params = )` (#280).

* `Chat$get_provider()` lets you access the underlying provider object, if needed (#202).

* `$extract_data()` now works better for arrays when `required = FALSE` (#384).

* `chat_claude()` and `chat_bedrock()` no longer choke after receiving an
  output that consists only of whitespace (#376).

* `live_browser()` now initializes `shinychat::chat_ui()` with the messages from
  the chat turns, rather than replaying the turns server-side (#381).

* `Chat$tokens()` is now called `Chat$get_tokens()` and returns a data frame of
  tokens, correctly aligned to the individual turn. The print method now uses
  this to show how many input/output tokens each turn used (#354).

* All requests now set a custom User-Agent that identifies that the requests
  comes from ellmer (#341).

* `provider_claude()` now supports `content_image_url()` (#347).

* `chat_claude()` gains `beta_header` argument to opt-in to beta features (#339).

* `chat_claude()` now supports `content_image_url()` (#347).

* `chat_claude()` now defaults to Sonnet 3.7 and displays the default
  model (#336).

* `Turn` objects now include a POSIXct timestamp in the `completed` slot that
  records when the turn was completed (#337, @simonpcouch).

* `create_tool_def()` can now use any Chat instance (#118, @pedrobtz).

* New experimental `parallel_chat()` and `parallel_chat_structured()` make it
  easier to perform multiple actions in parallel (#143). This is experimental
  because I'm not 100% sure that the shape of the user interface is correct,
  particularly as it pertains to handling errors.

* `google_upload()` lets you upload files to Google Gemini or Vertex AI (#310).

* `chat_gemini()` can now authenticate with Google default application
  credentials (including service accounts, etc). This requires the `gargle`
  package (#317, @atheriel).

* `chat_gemini()` now detects viewer-based credentials when running on Posit
  Connect (#320, @atheriel).

* `chat_ollama()` now works with `tool()` definitions with optional arguments or empty properties (#342, #348, @gadenbuie).

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
