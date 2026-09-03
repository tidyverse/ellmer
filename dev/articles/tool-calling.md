# Tool/function calling

## Introduction

One of the most interesting aspects of modern chat models is their
ability to make use of external tools that are defined by the caller.

When making a chat request to the chat model, the caller advertises one
or more tools (defined by their function name, description, and a list
of expected arguments), and the chat model can choose to respond with
one or more “tool calls”. These tool calls are requests *from the chat
model to the caller* to execute the function with the given arguments;
the caller is expected to execute the functions and “return” the results
by submitting another chat request with the conversation so far, plus
the results. The chat model can then use those results in formulating
its response, or, it may decide to make additional tool calls.

*Note that the chat model does not directly execute any external tools!*
It only makes requests for the caller to execute them. It’s easy to
think that tool calling might work like this:

![Diagram showing the wrong mental model of tool calls: a user initiates
a request that flows to the assistant, which then runs the code, and
returns the result back to the user.”](tool-calling-wrong.svg)

Diagram showing the wrong mental model of tool calls: a user initiates a
request that flows to the assistant, which then runs the code, and
returns the result back to the user.”

But in fact it works like this:

![Diagram showing the correct mental model for tool calls: a user sends
a request that needs a tool call, the assistant requests that the user
runs that tool, returns the result to the assistant, which uses it to
generate the final answer.](tool-calling-right.svg)

Diagram showing the correct mental model for tool calls: a user sends a
request that needs a tool call, the assistant requests that the user
runs that tool, returns the result to the assistant, which uses it to
generate the final answer.

The value that the chat model brings is not in helping with execution,
but with knowing when it makes sense to call a tool, what values to pass
as arguments, and how to use the results in formulating its response.

``` r

library(ellmer)
```

### Motivating example

Let’s take a look at an example where we really need an external tool.
Chat models generally do not know the current time, which makes
questions like these impossible.

``` r

chat <- chat_openai(model = "gpt-4o")
chat$chat("How long ago did Neil Armstrong touch down on the moon?")
#> Neil Armstrong touched down on the moon on July 20, 1969. As of 2023, 
#> that was 54 years ago.
```

Since the model doesn’t know what day it is, the result is incorrect.

### Defining a tool function

The first thing we’ll do is define an R function that returns the
current time.

``` r

#' Gets the current time in the given time zone.
#'
#' @param tz The time zone to get the current time in.
#' @return The current time in the given time zone.
get_current_time <- function(tz = "UTC") {
  format(Sys.time(), tz = tz, usetz = TRUE)
}
```

Note that we’ve gone through the trouble of creating [roxygen2
comments](https://roxygen2.r-lib.org/). This isn’t necessary, but as
we’ll see shortly, can make it a bit easier to generate a tool
defintion.

To turn a function into a tool, we provide some additional metadata that
the model will use:

``` r

get_current_time <- tool(
  get_current_time,
  name = "get_current_time",
  description = "Returns the current time.",
  arguments = list(
    tz = type_string(
      "Time zone to display the current time in. Defaults to `\"UTC\"`.",
      required = FALSE
    )
  )
)
```

This is a fair amount of code to write, even for such a simple function.
Fortunately, you don’t have to write this by hand! I generated the above
[`tool()`](https://ellmer.tidyverse.org/dev/reference/tool.md) call by
calling `create_tool_def(get_current_time)`, which uses an LLM to
generate the
[`tool()`](https://ellmer.tidyverse.org/dev/reference/tool.md) call for
you.
[`create_tool_def()`](https://ellmer.tidyverse.org/dev/reference/create_tool_def.md)
is not perfect, so you must review the generated code before using it,
but it is a big time-saver.

Note that a tool is just a special type of function so we can still call
it:

``` r

get_current_time()
#> [1] "2025-06-25 16:53:23 UTC"
```

### Registering and using tools

Now we need to give our chat object access to our tool. We do this with
`$register_tool()`:

``` r

chat$register_tool(get_current_time)
```

That’s all we need to do! Let’s retry our query:

``` r

chat$chat("How long ago did Neil Armstrong touch down on the moon?")
#> Neil Armstrong touched down on the moon on July 20, 1969. As of June 
#> 25, 2025, that was almost 56 years ago.
```

That’s correct! Without any further guidance, the chat model decided to
call our tool function and successfully used its result in formulating
its response.

If we print the chat we can see where the model decided to use the tool:

``` r

chat
#> <Chat OpenAI/gpt-4o turns=6 input=286 output=82 cost=$0.00>
#> ── user ───────────────────────────────────────────────────────────────
#> How long ago did Neil Armstrong touch down on the moon?
#> ── assistant [input=19 output=30 cost=$0.00] ──────────────────────────
#> Neil Armstrong touched down on the moon on July 20, 1969. As of 2023, that was 54 years ago.
#> ── user ───────────────────────────────────────────────────────────────
#> How long ago did Neil Armstrong touch down on the moon?
#> ── assistant [input=116 output=16 cost=$0.00] ─────────────────────────
#> [tool request (fc_0f19f871ea49202b016a6fc5b589bc81968b2a20c52239f613)]: get_current_time(tz = "UTC")
#> ── user ───────────────────────────────────────────────────────────────
#> [tool result  (fc_0f19f871ea49202b016a6fc5b589bc81968b2a20c52239f613)]: 2025-06-25 16:53:23 UTC
#> ── assistant [input=151 output=36 cost=$0.00] ─────────────────────────
#> Neil Armstrong touched down on the moon on July 20, 1969. As of June 25, 2025, that was almost 56 years ago.
```

(Full disclosure: I originally tried this example with the default model
of `gpt-4o-mini` and it got the tool calling right but the date math
wrong, hence the explicit `model="gpt-4o"`.)

This tool example was extremely simple, but you can imagine doing much
more interesting things from tool functions: calling APIs, reading from
or writing to a database, kicking off a complex simulation, or even
calling a complementary GenAI model (like an image generator). Or if you
are using ellmer in a Shiny app, you could use tools to set reactive
values, setting off a chain of reactive updates.

### Tool inputs and outputs

Remember that tool arguments come from the LLM, and tool results are
returned to the LLM. This implies that you should keep both as simple as
possible.

Inputs to a tool call, must be defined by
[`type_boolean()`](https://ellmer.tidyverse.org/dev/reference/type_boolean.md),
[`type_integer()`](https://ellmer.tidyverse.org/dev/reference/type_boolean.md),
[`type_number()`](https://ellmer.tidyverse.org/dev/reference/type_boolean.md),
[`type_string()`](https://ellmer.tidyverse.org/dev/reference/type_boolean.md),
[`type_enum()`](https://ellmer.tidyverse.org/dev/reference/type_boolean.md),
[`type_array()`](https://ellmer.tidyverse.org/dev/reference/type_boolean.md),
or
[`type_object()`](https://ellmer.tidyverse.org/dev/reference/type_boolean.md).
We recommend keeping them as simple as possible, focusing on basic
scalar types as much as you can.

The output of the tool call will be interpreted by the LLM, just as if
you had typed that information into the chat. Tool functions should
return a string, an atomic vector, a JSON string (e.g. from
[`jsonlite::toJSON()`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html)),
or a `Content` object.

For complex data like data frames or lists, use
[`jsonlite::toJSON()`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html)
to convert them explicitly before returning. Returning JSON doesn’t let
the model do anything extra with the data; unlike a program, it can’t
parse the JSON and compute with the values. If the answer requires exact
computation, compute it in the tool; the model’s job is only to read the
result.

To show off these ideas, here’s a slightly more complicated example
simulating a weather API that returns data for multiple cities at once.
The `get_weather()` function returns a data frame converted to JSON in
row-major format, which our experiments suggest is good for LLMs.

``` r

get_weather <- tool(
  function(cities) {
    raining <- c(London = "heavy", Houston = "none", Chicago = "overcast")
    temperature <- c(London = "cool", Houston = "hot", Chicago = "warm")
    wind <- c(London = "strong", Houston = "weak", Chicago = "strong")

    df <- data.frame(
      city = cities,
      raining = unname(raining[cities]),
      temperature = unname(temperature[cities]),
      wind = unname(wind[cities])
    )
    jsonlite::toJSON(df, auto_unbox = TRUE)
  },
  name = "get_weather",
  description = "
    Report on weather conditions in multiple cities. For efficiency, request
    all weather updates using a single tool call
  ",
  arguments = list(
    cities = type_array(type_string(), "City names")
  )
)
```

Now we register and use it:

``` r

chat <- chat_openai()
#> Using model = "gpt-5.6-terra".
chat$register_tool(get_weather)
chat$chat("Give me a weather update for London and Chicago")
#> - **London:** Heavy rain, cool temperatures, and strong winds. Bring 
#> waterproof layers and expect difficult conditions outdoors.
#> - **Chicago:** Overcast, warm, and windy. No rain reported, but gusty 
#> conditions are likely.
```

We can print the chat to confirm that the model only performed a single
tool call:

``` r

chat
#> <Chat OpenAI/gpt-5.6-terra turns=4 input=214 output=71 cost=$0.00>
#> ── user ───────────────────────────────────────────────────────────────
#> Give me a weather update for London and Chicago
#> ── assistant [input=72 output=21 cost=$0.00] ──────────────────────────
#> [tool request (fc_067a2e7696ea5827016a6fc5b7e76c8196894bff386923330d)]: get_weather(cities = c("London", "Chicago"))
#> ── user ───────────────────────────────────────────────────────────────
#> [tool result  (fc_067a2e7696ea5827016a6fc5b7e76c8196894bff386923330d)]: [{"city":"London","raining":"heavy","temperature":"cool","wind":"strong"},{"city":"Chicago","raining":"overcast","temperature":"warm","wind":"strong"}]
#> ── assistant [input=142 output=50 cost=$0.00] ─────────────────────────
#> - **London:** Heavy rain, cool temperatures, and strong winds. Bring waterproof layers and expect difficult conditions outdoors.
#> - **Chicago:** Overcast, warm, and windy. No rain reported, but gusty conditions are likely.
```

### Image and PDF tool output

ellmer allow tools to return image or PDF content that can be returned
with the tool result, if the LLM or API supports vision capabilities.

Simply return a
[`content_image_file()`](https://ellmer.tidyverse.org/dev/reference/content_image_url.md),
[`content_pdf_file()`](https://ellmer.tidyverse.org/dev/reference/content_pdf_file.md),
or similar content type from the tool function. For example, here’s a
simple tool to screenshot a website:

``` r

screenshot_website <- tool(
  function(url) {
    tmpf <- withr::local_tempfile(fileext = ".png")
    webshot2::webshot(url, file = tmpf)
    content_image_file(tmpf)
  },
  name = "screenshot_website",
  description = "Take a screenshot of a website.",
  arguments = list(
    url = type_string("The URL of the website")
  )
)
```

You could use this tool to allow the LLM to “see” websites, like [the
tidyverse website](https://tidyverse.org):

``` r

chat <- chat_openai()
#> Using model = "gpt-4.1".
chat$register_tool(screenshot_website)
chat$chat("Describe the design aesthetic of https://tidyverse.org")
#> https://tidyverse.org screenshot completed
#> The design aesthetic of the Tidyverse website (https://tidyverse.org) is
#> clean, modern, and minimalistic, with several distinct features:
#>
#> - **Color Palette**: The overall site uses a lot of white space with navy
#>   and dark backgrounds for some elements, accentuated by the colorful
#>   hexagonal logos for various R packages.
#> - **Typography**: Simple, sans-serif fonts contribute to readability and
#>   a contemporary look.
#> - **Hexagonal Icons**: Prominent display of tidyverse package logos in
#>   hexagonal shapes, emphasizing the modular, package-oriented
#>   nature of the Tidyverse.
#> - **Layout**: A balanced, spacious two-column layout. The left side
#>   features graphic elements; the right side provides concise, text-based
#>   information.
#>
#> Overall, the design communicates clarity, ease of use, and a focus on
#> modern data science tools.
```

### Tool context

A tool sometimes needs to know a little about the call it’s part of,
such as the tool request that triggered it or the conversation so far,
without that information being part of its arguments. Inside a tool
body,
[`tool_context()`](https://ellmer.tidyverse.org/dev/reference/tool_context.md)
returns a read-only context object with two fields:

- `$request`: the `ContentToolRequest` for this call, including `@id`,
  `@name`, and `@arguments`.
- `$turns`: a snapshot of the conversation history up to and including
  the assistant turn that issued this request (a list of `Turn`
  objects).

This is handy for logging or correlating tool calls with the request
that produced them:

``` r

report_tool <- tool(
  function(query) {
    ctx <- tool_context()
    cli::cli_alert_info("Running report for request {ctx$request@id}")
    run_report(query)
  },
  description = "Run a report for the given query.",
  arguments = list(query = type_string("The report query."))
)
```

In an async tool,
[`tool_context()`](https://ellmer.tidyverse.org/dev/reference/tool_context.md)
is only valid during the synchronous prefix of the function, in other
words before the first `await()`. Capture it at the top of your tool
body:

``` r

my_async_tool <- tool(
  coro::async(function(query) {
    ctx <- tool_context()          # capture before any await()
    result <- await(run_report_async(query))
    cli::cli_alert_info("Finished request {ctx$request@id}")
    result
  }),
  description = "Run a report asynchronously.",
  arguments = list(query = type_string("The report query."))
)
```

[`tool_context()`](https://ellmer.tidyverse.org/dev/reference/tool_context.md)
errors with a clear message if called after an `await()` or outside a
tool invocation. Built-in provider tools (such as
[`claude_tool_web_search()`](https://ellmer.tidyverse.org/dev/reference/claude_tool_web_search.md))
run provider-side and do not support
[`tool_context()`](https://ellmer.tidyverse.org/dev/reference/tool_context.md);
MCP tools do.

### Sharing state across tools

Related tools may need to share private state that the model should
never see, such as a database connection, an authenticated user, or a
running tally. A clean way to organise this is to bundle the tools
together as methods of an R6 class, with the shared state living in the
object’s fields. Each tool reads and writes that state through `self`,
and a `$tools()` method returns the ellmer tools.

``` r

Assistant <- R6::R6Class(
  "Assistant",
  public = list(
    initialize = function(user_email) {
      private$user_email <- user_email
    },

    # Return the ellmer tools, each wrapping a bound method. `tool()` can't
    # infer a name from `self$method`, so we pass `name` explicitly.
    tools = function() {
      list(
        tool(
          self$add_note,
          name = "add_note",
          description = "Save a note for the current user.",
          arguments = list(text = type_string("The note to save."))
        ),
        tool(
          self$list_notes,
          name = "list_notes",
          description = "List the current user's saved notes."
        )
      )
    },

    add_note = function(text) {
      private$notes <- c(private$notes, text)
      sprintf("Saved. You now have %d note(s).", length(private$notes))
    },

    list_notes = function() {
      if (length(private$notes) == 0) {
        return("No notes yet.")
      }
      private$notes
    }
  ),
  private = list(
    user_email = NULL,
    notes = character()
  )
)
```

Create the object once, register its tools, and the state accumulates
across turns and across `$chat()` calls, all without exposing
`user_email` or the raw notes to the model:

``` r

assistant <- Assistant$new(user_email = "hadley@posit.co")

chat <- chat_openai()
chat$register_tools(assistant$tools())

chat$chat("Remember that I need to buy milk.")
chat$chat("What do I need to do?")

# Inspect the state directly; this never passed through the model
assistant$list_notes()
#> [1] "buy milk"
```

Because the state lives in the object rather than in ellmer, you stay in
control of how it’s created, inspected, and persisted.
