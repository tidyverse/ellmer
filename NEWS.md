# ellmer (development version)

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

* `Chat$tokens()` now returns a data frame of tokens, correctly aligned to the
  individual turn. The print method now uses this to show how many input/output
  tokens each turn used (#354).

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

* New experimental `$chat_parallel()` and `$extract_data_parallel()` make it
  easier to perform multiple actions in parallel (#143). This is experimental
  because I'm not 100% sure that the shape of the user interface is correct,
  particularly as it pertains to handling errors.

  For Claude, note that the number of active connections is limited primarily
  by the output tokens per limit (OTPM) which is estimated from the `max_tokens` 
  parameter, which defaults to 4096. That means if you're limited to 16,000
  OPTM, you should use at most 16,000 / 4096 = ~4 active connections (or
  decrease `max_tokens`).

  Parallel calls with OpenAI and Gemini are much simpler in my experience.

* `gemini_upload()` lets you upload files to Gemini (#310).

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
