# Monitoring with OpenTelemetry

[OpenTelemetry](https://opentelemetry.io/) is an open standard for
recording what a program does, so you can see it later in an
observability tool like [Grafana](https://grafana.com/),
[Honeycomb](https://www.honeycomb.io/),
[Datadog](https://www.datadoghq.com/),
[Langfuse](https://langfuse.com/), or
[Logfire](https://pydantic.dev/logfire). ellmer records two kinds of
data:

- **Traces** show each step of a chat as a tree of timed *spans*: the
  call to `$chat()`, each request to the model, and each tool call.
- **Metrics** add up numbers across many chats, like request duration
  and token use.

ellmer follows the OpenTelemetry [semantic conventions for generative
AI](https://github.com/open-telemetry/semantic-conventions-genai), so
tools that understand those conventions can show ellmer’s data without
extra setup. This article shows you how to turn it on, what ellmer
records, and how to connect it to the rest of your app.

## Enable recording

ellmer uses the [otel](https://otel.r-lib.org) package to record data
and the [otelsdk](https://otelsdk.r-lib.org) package to send it. Make
sure both are installed:

``` r

pak::pak(c("otel", "otelsdk"))
```

Then tell otel where to send data using environment variables; otelsdk’s
[Collecting telemetry
data](https://otelsdk.r-lib.org/reference/collecting.html) page covers
this in more detail. Set the variables before R starts. You can edit
your R’s environment file using
[`usethis::edit_r_environ()`](https://usethis.r-lib.org/reference/edit.html).

To try things out, print spans to the console:

    OTEL_TRACES_EXPORTER=console

To send traces and metrics to an observability tool, use the
OpenTelemetry Protocol (OTLP) and point it at your tool or at an
[OpenTelemetry Collector](https://opentelemetry.io/docs/collector/). For
example:

    OTEL_TRACES_EXPORTER=otlp
    OTEL_METRICS_EXPORTER=otlp
    OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4318
    OTEL_SERVICE_NAME=my-app

Your tool’s docs will tell you which endpoint and headers to use. If no
exporter is set, ellmer records nothing and there is next to no cost.
See `?otel::environmentvariables` for all the options, including
`OTEL_R_SUPPRESS_SCOPES` to turn off data from one package. For example,
`OTEL_R_SUPPRESS_SCOPES=httr2` hides httr2’s `POST` spans but keeps
ellmer’s (requires `otel >= 0.2.1`).

Now that we have set up the environment, use ellmer as you normally
would:

``` r

library(ellmer)

chat <- chat_anthropic()
chat$chat("What's the capital of France?")
```

## What ellmer records

### Spans

Each call to `$chat()`, `$stream()`, or their async versions creates
this tree of spans:

    invoke_agent
    ├── chat claude-opus
    │   └── POST                       (from httr2)
    ├── execute_tool get_weather
    └── chat claude-opus
        └── POST

- `invoke_agent` covers the whole call, including every round of tool
  calls. It records the total tokens, the number of model requests
  (`gen_ai.invoke_agent.inference_calls`), and the number of tool calls
  (`gen_ai.invoke_agent.tool_calls`).

- `chat {model}` covers one request to the model. It records the
  provider (`gen_ai.provider.name`), the model you asked for and the
  model that answered, the parameters you set with
  [`params()`](https://ellmer.tidyverse.org/dev/reference/params.md)
  (`gen_ai.request.*`), token use (`gen_ai.usage.*`, including cached
  and reasoning tokens when the provider reports them), the finish
  reason, and, for streams, the time to the first chunk. Input tokens
  (`gen_ai.usage.input_tokens`) include cached input tokens, here and on
  `invoke_agent`.

- `execute_tool {name}` covers one tool call. It records the tool’s
  name, description, and call id, but not its arguments or result (see
  [Record prompts and responses](#record-prompts-and-responses)).

- `POST` comes from httr2, which ellmer uses to talk to the model. It
  records the HTTP details, like the URL and status code.

When code execution fails during a span, that span will set an error
status, an `error.type`, and (for R errors) an exception event. A failed
tool call only marks its `execute_tool` span: ellmer sends the error
back to the model and the chat goes on, so `invoke_agent` can still end
without an error. When a failure stops the whole call, like a failed
model request, `invoke_agent` gets the same error status and type. A
stream that you cancel with
[`stream_controller()`](https://ellmer.tidyverse.org/dev/reference/stream_controller.md)
has `error.type = "cancelled"`, and a call that stops early, such as
after a keyboard interrupt, has `error.type = "interrupted"`.

### Metrics

When `OTEL_METRICS_EXPORTER` is set, ellmer records these metrics. Model
and agent metrics are tagged with the operation, provider, and model,
and tool metrics with the tool name, so you can split them up in your
tool.

| Metric | What it measures |
|----|----|
| `gen_ai.client.inference.duration` | Time for each model request |
| `gen_ai.client.inference.time_to_first_chunk` | Time until a stream sends its first chunk |
| `gen_ai.client.inference.time_per_output_chunk` | Time between later chunks of a stream |
| `gen_ai.client.inference.operation.input_tokens` | Input tokens per request |
| `gen_ai.client.inference.operation.output_tokens` | Output tokens per request |
| `gen_ai.client.inference.usage.*` | Running totals of input, output, cached, and reasoning tokens |
| `gen_ai.execute_tool.duration` | Time for each tool call |
| `gen_ai.invoke_agent.duration` | Time for each `$chat()` call, including tool calls |
| `gen_ai.invoke_agent.inference_calls` | Model requests per `$chat()` call |
| `gen_ai.invoke_agent.tool_calls` | Tool calls per `$chat()` call |

## Record prompts and responses

By default, ellmer does not record the text of your prompts, the model’s
answers, or tool arguments and results, because they may hold private
data. To record them, set:

    OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT=true

Then each `chat` span also records the system prompt
(`gen_ai.system_instructions`), the conversation sent to the model
(`gen_ai.input.messages`), the model’s answer
(`gen_ai.output.messages`), and the tools it could call
(`gen_ai.tool.definitions`). Tool arguments and results are recorded
only as part of these messages: the model’s tool requests are in
`gen_ai.output.messages`, and the results are in the next `chat` span’s
`gen_ai.input.messages`. The `execute_tool` spans never record them.

Before you turn this on, check that your observability tool’s access and
retention settings are right for this data. ellmer does not shorten long
conversations, so a very long chat can make a span larger than some
tools accept.

## Group a conversation

Each request to the model is its own `chat` span. To tell your
observability tool that several of them belong to one conversation, set
a conversation id:

``` r

chat <- chat_anthropic()
chat$conversation_id <- "user-42-session-7"
chat$chat("Hi!")
chat$chat("What did I just say?")
```

ellmer records the id as `gen_ai.conversation.id` on the `invoke_agent`
and `chat` spans. Many tools use it to show a conversation as one
thread. Don’t put private data in the id; it’s recorded even when
message content isn’t.

## Add your own spans

ellmer’s spans become children of whatever span is active when you call
it. So to group them under a step in your own app, such as one request
to your API or one button click in a Shiny app, start your own span
around the call:

``` r

answer_question <- function(chat, question) {
  otel::start_local_active_span("handle question")
  chat$chat(question)
}
```

Now your `handle question` span is the parent of ellmer’s `invoke_agent`
span. This only works for `$chat()`: `start_local_active_span()` ends
the span when the function returns, which is before `$chat_async()` or
`$stream_async()` finish. For async calls, use
[`promises::with_otel_span()`](https://rstudio.github.io/promises/reference/otel.html),
which ends the span when the returned promise resolves:

``` r

answer_question_async <- function(chat, question) {
  promises::with_otel_promise_domain(
    promises::with_otel_span(
      "handle question",
      chat$chat_async(question),
      tracer = otel::get_tracer("my-app")
    )
  )
}
```

See
[`?promises::with_otel_span`](https://rstudio.github.io/promises/reference/otel.html)
for details. Other packages that use otel, like Shiny, record their own
spans too. Together, they give you observability over your whole app,
with ellmer’s spans naturally nested inside your app’s spans.
