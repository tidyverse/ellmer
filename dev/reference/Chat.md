# The Chat object

A `Chat` is a sequence of user and assistant
[Turn](https://ellmer.tidyverse.org/dev/reference/Turn.md)s sent to a
specific
[Provider](https://ellmer.tidyverse.org/dev/reference/Provider.md). A
`Chat` is a mutable R6 object that takes care of managing the state
associated with the chat; i.e. it records the messages that you send to
the server, and the messages that you receive back. If you register a
tool (i.e. an R function that the assistant can call on your behalf), it
also takes care of the tool loop.

You should generally not create this object yourself, but instead call
[`chat_openai()`](https://ellmer.tidyverse.org/dev/reference/chat_openai.md)
or friends instead.

## Value

A Chat object

## Active bindings

- `conversation_id`:

  Identifier for the current conversation. When set, it is recorded as
  the `gen_ai.conversation.id` attribute on the OpenTelemetry spans
  emitted for subsequent model calls. Assign `NULL` to clear.

  Developer-facing: intended for frameworks that manage conversation
  history (e.g., Shiny apps). ellmer never generates an identifier on
  its own.

## Methods

### Public methods

- [`Chat$new()`](#method-Chat-initialize)

- [`Chat$get_turns()`](#method-Chat-get_turns)

- [`Chat$set_turns()`](#method-Chat-set_turns)

- [`Chat$get_rounds()`](#method-Chat-get_rounds)

- [`Chat$last_round()`](#method-Chat-last_round)

- [`Chat$add_turn()`](#method-Chat-add_turn)

- [`Chat$get_system_prompt()`](#method-Chat-get_system_prompt)

- [`Chat$get_model()`](#method-Chat-get_model)

- [`Chat$get_model_object()`](#method-Chat-get_model_object)

- [`Chat$set_model()`](#method-Chat-set_model)

- [`Chat$set_system_prompt()`](#method-Chat-set_system_prompt)

- [`Chat$get_tokens()`](#method-Chat-get_tokens)

- [`Chat$get_cost()`](#method-Chat-get_cost)

- [`Chat$token_count()`](#method-Chat-token_count)

- [`Chat$file_upload()`](#method-Chat-file_upload)

- [`Chat$file_list()`](#method-Chat-file_list)

- [`Chat$file_get()`](#method-Chat-file_get)

- [`Chat$file_download()`](#method-Chat-file_download)

- [`Chat$file_delete()`](#method-Chat-file_delete)

- [`Chat$last_turn()`](#method-Chat-last_turn)

- [`Chat$chat()`](#method-Chat-chat)

- [`Chat$chat_structured()`](#method-Chat-chat_structured)

- [`Chat$chat_structured_async()`](#method-Chat-chat_structured_async)

- [`Chat$chat_async()`](#method-Chat-chat_async)

- [`Chat$stream()`](#method-Chat-stream)

- [`Chat$stream_async()`](#method-Chat-stream_async)

- [`Chat$register_tool()`](#method-Chat-register_tool)

- [`Chat$register_tools()`](#method-Chat-register_tools)

- [`Chat$get_provider()`](#method-Chat-get_provider)

- [`Chat$get_tools()`](#method-Chat-get_tools)

- [`Chat$set_tools()`](#method-Chat-set_tools)

- [`Chat$on_tool_request()`](#method-Chat-on_tool_request)

- [`Chat$on_tool_result()`](#method-Chat-on_tool_result)

- [`Chat$on_request_start()`](#method-Chat-on_request_start)

- [`Chat$on_request_end()`](#method-Chat-on_request_end)

- [`Chat$clone()`](#method-Chat-clone)

------------------------------------------------------------------------

### `Chat$new()`

#### Usage

    Chat$new(provider, model = NULL, system_prompt = NULL, echo = "none")

#### Arguments

- `provider`:

  A provider object.

- `model`:

  A [Model](https://ellmer.tidyverse.org/dev/reference/Model.md) object.

- `system_prompt`:

  System prompt to start the conversation with.

- `echo`:

  One of the following options:

  - `none`: don't emit any output (default when running in a function).

  - `output`: echo text and tool-calling output after the turn completes
    (default when running at the console).

  - `all`: echo all input and output.

  Console display occurs after a turn completes so ellmer can add
  citation markers and a source list to the response.

  Note this only affects the
  [`chat()`](https://ellmer.tidyverse.org/dev/reference/chat-any.md)
  method. You can override the default by setting the `ellmer_echo`
  option.

------------------------------------------------------------------------

### `Chat$get_turns()`

Retrieve the turns that have been sent and received so far (optionally
starting with the system prompt, if any).

#### Usage

    Chat$get_turns(include_system_prompt = FALSE)

#### Arguments

- `include_system_prompt`:

  Whether to include the system prompt in the turns (if any exists).

------------------------------------------------------------------------

### `Chat$set_turns()`

Replace existing turns with a new list.

#### Usage

    Chat$set_turns(value)

#### Arguments

- `value`:

  A list of [Turn](https://ellmer.tidyverse.org/dev/reference/Turn.md)s.

------------------------------------------------------------------------

### `Chat$get_rounds()`

Retrieve the conversation grouped into
[Round](https://ellmer.tidyverse.org/dev/reference/Round.md)s. Each
`Round` pairs a user turn with the assistant and tool-result turns it
produced.

#### Usage

    Chat$get_rounds(include_system_prompt = FALSE)

#### Arguments

- `include_system_prompt`:

  Whether to include system turns in the rounds. When `FALSE` (the
  default), all system turns are dropped. When `TRUE`, each system turn
  is folded into the `input` of the round it precedes.

------------------------------------------------------------------------

### `Chat$last_round()`

The last [Round](https://ellmer.tidyverse.org/dev/reference/Round.md) of
conversation. Note that system prompt turns are included, equivalent to
the last item in the list of rounds returned by
`$get_rounds(include_system_prompt = TRUE)`.

#### Usage

    Chat$last_round()

#### Returns

Either a `Round` or `NULL`, if no rounds have occurred.

------------------------------------------------------------------------

### `Chat$add_turn()`

Add a pair of turns to the chat.

#### Usage

    Chat$add_turn(user, assistant, log_tokens = TRUE)

#### Arguments

- `user`:

  The user [Turn](https://ellmer.tidyverse.org/dev/reference/Turn.md).

- `assistant`:

  The system [Turn](https://ellmer.tidyverse.org/dev/reference/Turn.md).

- `log_tokens`:

  Should tokens used in the turn be logged to the session counter?

------------------------------------------------------------------------

### `Chat$get_system_prompt()`

If set, the system prompt, it not, `NULL`.

#### Usage

    Chat$get_system_prompt()

------------------------------------------------------------------------

### `Chat$get_model()`

Retrieve the model name.

#### Usage

    Chat$get_model()

------------------------------------------------------------------------

### `Chat$get_model_object()`

Retrieve the Model object. For expert use only.

#### Usage

    Chat$get_model_object()

------------------------------------------------------------------------

### `Chat$set_model()`

Update the model name. Note that unlike some of the `chat_*()`
functions, the model name is not validated against available models for
the provider.

#### Usage

    Chat$set_model(model)

#### Arguments

- `model`:

  A single string giving the new model name.

------------------------------------------------------------------------

### `Chat$set_system_prompt()`

Update the system prompt

#### Usage

    Chat$set_system_prompt(value)

#### Arguments

- `value`:

  A character vector giving the new system prompt

------------------------------------------------------------------------

### `Chat$get_tokens()`

A data frame with token usage and cost data. There are four columns:
`input`, `output`, `cached_input`, and `cost`. There is one row for each
assistant turn, because token counts and costs are only available when
the API returns the assistant's response.

#### Usage

    Chat$get_tokens(include_system_prompt = deprecated())

#### Arguments

- `include_system_prompt`:

  **\[deprecated\]**

------------------------------------------------------------------------

### `Chat$get_cost()`

The cost of this chat

#### Usage

    Chat$get_cost(include = c("all", "last"))

#### Arguments

- `include`:

  The default, `"all"`, gives the total cumulative cost of this chat.
  Alternatively, use `"last"` to get the cost of just the most recent
  turn. Incomplete turns (from cancelled or interrupted streams) are
  excluded because they lack token data.

------------------------------------------------------------------------

### `Chat$token_count()`

Estimate the token count for `...` using the provider's token counting
endpoint.

#### Usage

    Chat$token_count(..., include = c("new", "complete"), type = NULL)

#### Arguments

- `...`:

  Input to count tokens for.

- `include`:

  What to include in the count. `"new"` counts tokens only for the
  contents of `...`. `"complete"` estimates the total input tokens for
  the next request, including system prompt, tools, and conversation
  history.

- `type`:

  An optional type specification for structured data extraction, created
  with a
  [`type_()`](https://ellmer.tidyverse.org/dev/reference/type_boolean.md)
  function.

#### Returns

The estimated number of input tokens.

------------------------------------------------------------------------

### `Chat$file_upload()`

**\[experimental\]**

Upload a file to the chat's provider, once, so later turns can reference
it by id instead of re-sending its contents. Prefer this over
[`content_pdf_file()`](https://ellmer.tidyverse.org/dev/reference/content_pdf_file.md),
[`content_image_file()`](https://ellmer.tidyverse.org/dev/reference/content_image_url.md),
or
[`content_document_file()`](https://ellmer.tidyverse.org/dev/reference/content_document_file.md)
when a file is large or used across many turns. Otherwise, sending the
file inline is simpler: it isn't limited to providers with a files API,
and there's nothing stored on the provider's side to expire or clean up.

File management is supported by
[`chat_openai()`](https://ellmer.tidyverse.org/dev/reference/chat_openai.md),
[`chat_anthropic()`](https://ellmer.tidyverse.org/dev/reference/chat_anthropic.md),
and
[`chat_google_gemini()`](https://ellmer.tidyverse.org/dev/reference/chat_google_gemini.md);
other providers error. Provider notes:

- Gemini files always expire after 48 hours (so `expires_in_h` can't be
  changed), and uploading waits until Gemini finishes processing the
  file (which can take a while for large video/audio), so the returned
  reference is always ready to use. The Files API isn't available on
  Vertex AI; there, upload the file to a Cloud Storage bucket and
  reference it with
  `ContentUploaded(uri = "gs://bucket/object", mime_type = ...)`.

- An OpenAI upload can also be referenced from a
  [`chat_openai_compatible()`](https://ellmer.tidyverse.org/dev/reference/chat_openai_compatible.md)
  chat pointed at OpenAI's Chat Completions API, except for images,
  which that API can't reference by id.

#### Usage

    Chat$file_upload(path, mime_type = NULL, expires_in_h = 48)

#### Arguments

- `path`:

  Path to a file to upload.

- `mime_type`:

  MIME type of the file. If not supplied, it's guessed from the file
  extension.

- `expires_in_h`:

  Number of hours until the provider deletes the file. Defaults to 48.
  Anthropic accepts 1 to 2160 (90 days), OpenAI 1 to 720 (30 days), and
  both accept `Inf` to keep the file until you delete it yourself.
  Gemini always uses 48 and can't be changed.

#### Returns

A
[ContentUploaded](https://ellmer.tidyverse.org/dev/reference/Content.md)
that can be passed to `$chat()` and friends in place of the file itself.

------------------------------------------------------------------------

### `Chat$file_list()`

**\[experimental\]**

List files previously uploaded to the chat's provider.

#### Usage

    Chat$file_list()

#### Returns

A data frame with one row per file: normalized columns (`id`,
`filename`, `mime_type`, `size_bytes`, `created_at`, `expires_at`)
first, then any provider-specific columns.

------------------------------------------------------------------------

### `Chat$file_get()`

**\[experimental\]**

Get a reference to a file previously uploaded to the chat's provider,
e.g. to reuse an upload from an earlier session. Use `$file_list()` to
find the id.

#### Usage

    Chat$file_get(id)

#### Arguments

- `id`:

  A file id string, or a
  [ContentUploaded](https://ellmer.tidyverse.org/dev/reference/Content.md).

#### Returns

A
[ContentUploaded](https://ellmer.tidyverse.org/dev/reference/Content.md)
that can be passed to `$chat()` and friends, with file metadata
(`filename`, `size_bytes`, `created_at`, `expires_at`, and any
provider-specific fields) in its `extra` property. OpenAI doesn't report
a file's MIME type, so it's guessed from the filename.

------------------------------------------------------------------------

### `Chat$file_download()`

**\[experimental\]**

Download a file from the chat's provider, writing it to `path`. Note
that providers only serve back model-generated files (e.g. batch
outputs); files you uploaded yourself can't be re-downloaded.

#### Usage

    Chat$file_download(id, path)

#### Arguments

- `id`:

  A file id string, or a
  [ContentUploaded](https://ellmer.tidyverse.org/dev/reference/Content.md).

- `path`:

  Path to write the downloaded file to.

#### Returns

`path`, invisibly.

------------------------------------------------------------------------

### `Chat$file_delete()`

**\[experimental\]**

Delete a file previously uploaded to the chat's provider.

#### Usage

    Chat$file_delete(id)

#### Arguments

- `id`:

  A file id string, or a
  [ContentUploaded](https://ellmer.tidyverse.org/dev/reference/Content.md).

------------------------------------------------------------------------

### `Chat$last_turn()`

The last turn returned by the assistant.

#### Usage

    Chat$last_turn(role = c("assistant", "user", "system"))

#### Arguments

- `role`:

  Optionally, specify a role to find the last turn with for the role.

#### Returns

Either a `Turn` or `NULL`, if no turns with the specified role have
occurred.

------------------------------------------------------------------------

### `Chat$chat()`

Submit input to the chatbot, and return the response as a simple string
(probably Markdown).

#### Usage

    Chat$chat(..., echo = NULL)

#### Arguments

- `...`:

  The input to send to the chatbot. Can be strings or images (see
  [`content_image_file()`](https://ellmer.tidyverse.org/dev/reference/content_image_url.md)
  and
  [`content_image_url()`](https://ellmer.tidyverse.org/dev/reference/content_image_url.md).

- `echo`:

  Whether to emit the response to stdout as it is received. If `NULL`,
  then the value of `echo` set when the chat object was created will be
  used.

------------------------------------------------------------------------

### `Chat$chat_structured()`

Extract structured data.

Note: tool calling is disabled during structured data extraction. See
[`vignette("structured-data")`](https://ellmer.tidyverse.org/dev/articles/structured-data.md)
for details and workarounds.

#### Usage

    Chat$chat_structured(..., type, echo = "none", convert = TRUE)

#### Arguments

- `...`:

  The input to send to the chatbot. This is typically the text you want
  to extract data from, but it can be omitted if the data is obvious
  from the existing conversation.

- `type`:

  A type specification for the extracted data. Should be created with a
  [`type_()`](https://ellmer.tidyverse.org/dev/reference/type_boolean.md)
  function.

- `echo`:

  Whether to emit the response to stdout as it is received. Set to
  "text" to stream JSON data as it's generated (not supported by all
  providers).

- `convert`:

  Automatically convert from JSON lists to R data types using the
  schema. For example, this will turn arrays of objects into data frames
  and arrays of strings into a character vector.

------------------------------------------------------------------------

### `Chat$chat_structured_async()`

Extract structured data, asynchronously. Returns a promise that resolves
to an object matching the type specification.

#### Usage

    Chat$chat_structured_async(..., type, echo = "none", convert = TRUE)

#### Arguments

- `...`:

  The input to send to the chatbot. Will typically include the phrase
  "extract structured data".

- `type`:

  A type specification for the extracted data. Should be created with a
  [`type_()`](https://ellmer.tidyverse.org/dev/reference/type_boolean.md)
  function.

- `echo`:

  Whether to emit the response to stdout as it is received. Set to
  "text" to stream JSON data as it's generated (not supported by all
  providers).

- `convert`:

  Automatically convert from JSON lists to R data types using the
  schema. For example, this will turn arrays of objects into data frames
  and arrays of strings into a character vector.

------------------------------------------------------------------------

### `Chat$chat_async()`

Submit input to the chatbot, and receive a promise that resolves with
the response all at once. Returns a promise that resolves to a string
(probably Markdown).

#### Usage

    Chat$chat_async(..., tool_mode = c("concurrent", "sequential"))

#### Arguments

- `...`:

  The input to send to the chatbot. Can be strings or images.

- `tool_mode`:

  Whether tools should be invoked one-at-a-time (`"sequential"`) or
  concurrently (`"concurrent"`). Sequential mode is best for interactive
  applications, especially when a tool may involve an interactive user
  interface. Concurrent mode is the default and is best suited for
  automated scripts or non-interactive applications.

------------------------------------------------------------------------

### `Chat$stream()`

Submit input to the chatbot, returning streaming results. Returns A
[coro
generator](https://coro.r-lib.org/articles/generator.html#iterating)
that yields strings. While iterating, the generator will block while
waiting for more content from the chatbot.

#### Usage

    Chat$stream(..., type = NULL, stream = c("text", "content"), controller = NULL)

#### Arguments

- `...`:

  The input to send to the chatbot. Can be strings or images.

- `type`:

  An optional `type_()` structured-data specification. When supplied,
  registered tools are suppressed and the completed assistant turn
  stores a `ContentJson`. The provider constrains the response to JSON.
  With `stream = "text"` (the default), structured stream chunks are raw
  JSON text; with `stream = "content"`, they are
  [Content](https://ellmer.tidyverse.org/dev/reference/Content.md)
  objects. Streaming structured output requires native provider support;
  tool-based fallback is not supported.

- `stream`:

  Whether the stream should yield only `"text"` or ellmer's rich content
  types. When `stream = "content"`, `stream()` yields
  [Content](https://ellmer.tidyverse.org/dev/reference/Content.md)
  objects.

- `controller`:

  An optional
  [`stream_controller()`](https://ellmer.tidyverse.org/dev/reference/stream_controller.md)
  used to cancel the stream from outside the iteration loop.

------------------------------------------------------------------------

### `Chat$stream_async()`

Submit input to the chatbot, returning asynchronously streaming results.
Returns a [coro async
generator](https://coro.r-lib.org/reference/async_generator.html) that
yields string promises.

#### Usage

    Chat$stream_async(
      ...,
      type = NULL,
      tool_mode = c("concurrent", "sequential"),
      stream = c("text", "content"),
      controller = NULL
    )

#### Arguments

- `...`:

  The input to send to the chatbot. Can be strings or images.

- `type`:

  An optional `type_()` structured-data specification. When supplied,
  registered tools are suppressed and the completed assistant turn
  stores a `ContentJson`. The provider constrains the response to JSON.
  With `stream = "text"` (the default), structured stream chunks are raw
  JSON text; with `stream = "content"`, they are
  [Content](https://ellmer.tidyverse.org/dev/reference/Content.md)
  objects. Streaming structured output requires native provider support;
  tool-based fallback is not supported.

- `tool_mode`:

  Whether tools should be invoked one-at-a-time (`"sequential"`) or
  concurrently (`"concurrent"`). Sequential mode is best for interactive
  applications, especially when a tool may involve an interactive user
  interface. Concurrent mode is the default and is best suited for
  automated scripts or non-interactive applications.

- `stream`:

  Whether the stream should yield only `"text"` or ellmer's rich content
  types. When `stream = "content"`, `stream()` yields
  [Content](https://ellmer.tidyverse.org/dev/reference/Content.md)
  objects.

- `controller`:

  An optional
  [`stream_controller()`](https://ellmer.tidyverse.org/dev/reference/stream_controller.md)
  used to cancel the stream from outside the iteration loop.

------------------------------------------------------------------------

### `Chat$register_tool()`

Register a tool (an R function) that the chatbot can use. Learn more in
[`vignette("tool-calling")`](https://ellmer.tidyverse.org/dev/articles/tool-calling.md).

#### Usage

    Chat$register_tool(tool)

#### Arguments

- `tool`:

  A tool definition created by
  [`tool()`](https://ellmer.tidyverse.org/dev/reference/tool.md).

------------------------------------------------------------------------

### `Chat$register_tools()`

Register a list of tools. Learn more in
[`vignette("tool-calling")`](https://ellmer.tidyverse.org/dev/articles/tool-calling.md).

#### Usage

    Chat$register_tools(tools)

#### Arguments

- `tools`:

  A list of tool definitions created by
  [`tool()`](https://ellmer.tidyverse.org/dev/reference/tool.md).

------------------------------------------------------------------------

### `Chat$get_provider()`

Get the underlying provider object. For expert use only.

#### Usage

    Chat$get_provider()

------------------------------------------------------------------------

### `Chat$get_tools()`

Retrieve the list of registered tools.

#### Usage

    Chat$get_tools()

------------------------------------------------------------------------

### `Chat$set_tools()`

Sets the available tools. For expert use only; most users should use
`register_tool()`.

#### Usage

    Chat$set_tools(tools)

#### Arguments

- `tools`:

  A list of tool definitions created with
  [`tool()`](https://ellmer.tidyverse.org/dev/reference/tool.md).

------------------------------------------------------------------------

### `Chat$on_tool_request()`

Register a callback for a tool request event.

#### Usage

    Chat$on_tool_request(callback)

#### Arguments

- `callback`:

  A function to be called when a tool request event occurs, which must
  have `request` as its only argument.

#### Returns

A function that can be called to remove the callback.

------------------------------------------------------------------------

### `Chat$on_tool_result()`

Register a callback for a tool result event.

#### Usage

    Chat$on_tool_result(callback)

#### Arguments

- `callback`:

  A function to be called when a tool result event occurs, which must
  have `result` as its only argument.

#### Returns

A function that can be called to remove the callback.

------------------------------------------------------------------------

### `Chat$on_request_start()`

Register a callback that fires before each model request, including each
round of the tool loop. Use it to inspect the outgoing request, or to
compact the conversation with `$set_turns()`.

`turns` includes the pending turn about to be sent, which `$set_turns()`
re-appends automatically. So compact with
`chat$set_turns(compact(chat$get_turns()))` rather than passing `turns`
back to `$set_turns()`, which would duplicate the pending turn.

#### Usage

    Chat$on_request_start(callback)

#### Arguments

- `callback`:

  A function called with a single argument `turns`, the list of turns
  about to be sent. The return value is ignored, but may be a promise
  when used with `$chat_async()` or `$stream_async()`.

#### Returns

A function that can be called to remove the callback.

------------------------------------------------------------------------

### `Chat$on_request_end()`

Register a callback that fires after each model request, before any tool
calls in the response are executed. Use it to track latency or cost per
request, or to observe tool requests before they run.

If the request is cancelled, `turn` is an
[AssistantPartialTurn](https://ellmer.tidyverse.org/dev/reference/Turn.md)
with `NA` tokens and cost. If the request errors, the callback does not
fire.

#### Usage

    Chat$on_request_end(callback)

#### Arguments

- `callback`:

  A function called with a single argument `turn`, the assistant turn
  just returned by the model. The return value is ignored, but may be a
  promise when used with `$chat_async()` or `$stream_async()`.

#### Returns

A function that can be called to remove the callback.

------------------------------------------------------------------------

### `Chat$clone()`

The objects of this class are cloneable with this method.

#### Usage

    Chat$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
chat <- chat_openai()
#> Using model = "gpt-5.6-terra".
chat$chat("Tell me a funny joke")
#> Why don’t skeletons fight each other?
#> 
#> Because they don’t have the guts.
```
