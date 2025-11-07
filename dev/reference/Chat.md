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

## Methods

### Public methods

- [`Chat$new()`](#method-Chat-new)

- [`Chat$get_turns()`](#method-Chat-get_turns)

- [`Chat$set_turns()`](#method-Chat-set_turns)

- [`Chat$add_turn()`](#method-Chat-add_turn)

- [`Chat$get_system_prompt()`](#method-Chat-get_system_prompt)

- [`Chat$get_model()`](#method-Chat-get_model)

- [`Chat$set_system_prompt()`](#method-Chat-set_system_prompt)

- [`Chat$get_tokens()`](#method-Chat-get_tokens)

- [`Chat$get_cost()`](#method-Chat-get_cost)

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

- [`Chat$clone()`](#method-Chat-clone)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    Chat$new(provider, system_prompt = NULL, echo = "none")

#### Arguments

- `provider`:

  A provider object.

- `system_prompt`:

  System prompt to start the conversation with.

- `echo`:

  One of the following options:

  - `none`: don't emit any output (default when running in a function).

  - `output`: echo text and tool-calling output as it streams in
    (default when running at the console).

  - `all`: echo all input and output.

  Note this only affects the
  [`chat()`](https://ellmer.tidyverse.org/dev/reference/chat-any.md)
  method. You can override the default by setting the `ellmer_echo`
  option.

------------------------------------------------------------------------

### Method `get_turns()`

Retrieve the turns that have been sent and received so far (optionally
starting with the system prompt, if any).

#### Usage

    Chat$get_turns(include_system_prompt = FALSE)

#### Arguments

- `include_system_prompt`:

  Whether to include the system prompt in the turns (if any exists).

------------------------------------------------------------------------

### Method `set_turns()`

Replace existing turns with a new list.

#### Usage

    Chat$set_turns(value)

#### Arguments

- `value`:

  A list of [Turn](https://ellmer.tidyverse.org/dev/reference/Turn.md)s.

------------------------------------------------------------------------

### Method `add_turn()`

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

### Method `get_system_prompt()`

If set, the system prompt, it not, `NULL`.

#### Usage

    Chat$get_system_prompt()

------------------------------------------------------------------------

### Method `get_model()`

Retrieve the model name

#### Usage

    Chat$get_model()

------------------------------------------------------------------------

### Method `set_system_prompt()`

Update the system prompt

#### Usage

    Chat$set_system_prompt(value)

#### Arguments

- `value`:

  A character vector giving the new system prompt

------------------------------------------------------------------------

### Method `get_tokens()`

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

### Method `get_cost()`

The cost of this chat

#### Usage

    Chat$get_cost(include = c("all", "last"))

#### Arguments

- `include`:

  The default, `"all"`, gives the total cumulative cost of this chat.
  Alternatively, use `"last"` to get the cost of just the most recent
  turn.

------------------------------------------------------------------------

### Method `last_turn()`

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

### Method [`chat()`](https://ellmer.tidyverse.org/dev/reference/chat-any.md)

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

### Method `chat_structured()`

Extract structured data

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

### Method `chat_structured_async()`

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

### Method `chat_async()`

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

### Method `stream()`

Submit input to the chatbot, returning streaming results. Returns A
[coro
generator](https://coro.r-lib.org/articles/generator.html#iterating)
that yields strings. While iterating, the generator will block while
waiting for more content from the chatbot.

#### Usage

    Chat$stream(..., stream = c("text", "content"))

#### Arguments

- `...`:

  The input to send to the chatbot. Can be strings or images.

- `stream`:

  Whether the stream should yield only `"text"` or ellmer's rich content
  types. When `stream = "content"`, `stream()` yields
  [Content](https://ellmer.tidyverse.org/dev/reference/Content.md)
  objects.

------------------------------------------------------------------------

### Method `stream_async()`

Submit input to the chatbot, returning asynchronously streaming results.
Returns a [coro async
generator](https://coro.r-lib.org/reference/async_generator.html) that
yields string promises.

#### Usage

    Chat$stream_async(
      ...,
      tool_mode = c("concurrent", "sequential"),
      stream = c("text", "content")
    )

#### Arguments

- `...`:

  The input to send to the chatbot. Can be strings or images.

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

------------------------------------------------------------------------

### Method `register_tool()`

Register a tool (an R function) that the chatbot can use. Learn more in
[`vignette("tool-calling")`](https://ellmer.tidyverse.org/dev/articles/tool-calling.md).

#### Usage

    Chat$register_tool(tool)

#### Arguments

- `tool`:

  A tool definition created by
  [`tool()`](https://ellmer.tidyverse.org/dev/reference/tool.md).

------------------------------------------------------------------------

### Method `register_tools()`

Register a list of tools. Learn more in
[`vignette("tool-calling")`](https://ellmer.tidyverse.org/dev/articles/tool-calling.md).

#### Usage

    Chat$register_tools(tools)

#### Arguments

- `tools`:

  A list of tool definitions created by
  [`tool()`](https://ellmer.tidyverse.org/dev/reference/tool.md).

------------------------------------------------------------------------

### Method `get_provider()`

Get the underlying provider object. For expert use only.

#### Usage

    Chat$get_provider()

------------------------------------------------------------------------

### Method `get_tools()`

Retrieve the list of registered tools.

#### Usage

    Chat$get_tools()

------------------------------------------------------------------------

### Method `set_tools()`

Sets the available tools. For expert use only; most users should use
`register_tool()`.

#### Usage

    Chat$set_tools(tools)

#### Arguments

- `tools`:

  A list of tool definitions created with
  [`tool()`](https://ellmer.tidyverse.org/dev/reference/tool.md).

------------------------------------------------------------------------

### Method `on_tool_request()`

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

### Method `on_tool_result()`

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

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Chat$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
chat <- chat_openai()
#> Using model = "gpt-4.1".
chat$chat("Tell me a funny joke")
#> Sure! Here you go:
#> 
#> Why did the scarecrow win an award?
#> 
#> Because he was outstanding in his field! 🌾😄
```
