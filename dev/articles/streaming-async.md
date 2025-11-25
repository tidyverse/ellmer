# Streaming and async APIs

### Streaming results

The [`chat()`](https://ellmer.tidyverse.org/dev/reference/chat-any.md)
method does not return any results until the entire response is
received. (It can *print* the streaming results to the console but it
*returns* the result only when the response is complete.)

If you want to process the response as it arrives, you can use the
`stream()` method. This is useful when you want to send the response, in
realtime, somewhere other than the R console (e.g., to a file, an HTTP
response, or a Shiny chat window), or when you want to manipulate the
response before displaying it without giving up the immediacy of
streaming.

With the `stream()` method, which returns a
[coro](https://coro.r-lib.org/)
[generator](https://coro.r-lib.org/articles/generator.html), you can
process the response by looping over it as it arrives.

``` r
stream <- chat$stream("What are some common uses of R?")
coro::loop(for (chunk in stream) {
  cat(toupper(chunk))
})
#>  R IS COMMONLY USED FOR:
#>
#>  1. **STATISTICAL ANALYSIS**: PERFORMING COMPLEX STATISTICAL TESTS AND ANALYSES.
#>  2. **DATA VISUALIZATION**: CREATING GRAPHS, CHARTS, AND PLOTS USING PACKAGES LIKE  GGPLOT2.
#>  3. **DATA MANIPULATION**: CLEANING AND TRANSFORMING DATA WITH PACKAGES LIKE DPLYR AND TIDYR.
#>  4. **MACHINE LEARNING**: BUILDING PREDICTIVE MODELS WITH LIBRARIES LIKE CARET AND #>  RANDOMFOREST.
#>  5. **BIOINFORMATICS**: ANALYZING BIOLOGICAL DATA AND GENOMIC STUDIES.
#>  6. **ECONOMETRICS**: PERFORMING ECONOMIC DATA ANALYSIS AND MODELING.
#>  7. **REPORTING**: GENERATING DYNAMIC REPORTS AND DASHBOARDS WITH R MARKDOWN.
#>  8. **TIME SERIES ANALYSIS**: ANALYZING TEMPORAL DATA AND FORECASTING.
#>
#>  THESE USES MAKE R A POWERFUL TOOL FOR DATA SCIENTISTS, STATISTICIANS, AND RESEARCHERS.
```

## Async usage

ellmer also supports async usage. This is useful when you want to run
multiple, concurrent chat sessions. This is particularly important for
Shiny applications where using the methods described above would block
the Shiny app for other users for the duration of each response.

To use async chat, call `chat_async()`/`stream_async()` instead of
[`chat()`](https://ellmer.tidyverse.org/dev/reference/chat-any.md)/`stream()`.
The `_async` variants take the same arguments for construction but
return a promise instead of the actual response.

Remember that chat objects are stateful; they preserve the conversation
history as you interact with it. This means that it doesn’t make sense
to issue multiple, concurrent chat/stream operations on the same chat
object because the conversation history can become corrupted with
interleaved conversation fragments. If you need to run concurrent chat
sessions, create multiple chat objects.

### Asynchronous chat

For asynchronous, non-streaming chat, you’d use the
[`chat()`](https://ellmer.tidyverse.org/dev/reference/chat-any.md)
method as before, but handle the result as a promise instead of a
string.

``` r
library(promises)

chat$chat_async("How's your day going?") %...>% print()
#> I'm just a computer program, so I don't have feelings, but I'm here to help you with any questions you have.
```

#### Shiny example

To add an asynchronous chat interface in your Shiny application, we
recommend using [the shinychat
package](https://posit-dev.github.io/shinychat/).

The simplest approach is to use shinychat’s Shiny module to add a chat
UI to your app—similar to the app created by
[`live_browser()`](https://ellmer.tidyverse.org/dev/reference/live_console.md)—using
the
[`shinychat::chat_mod_ui()`](https://posit-dev.github.io/shinychat/r/reference/chat_app.html)
and
[`shinychat::chat_mod_server()`](https://posit-dev.github.io/shinychat/r/reference/chat_app.html)
functions. These module functions connect an
[`ellmer::Chat`](https://ellmer.tidyverse.org/dev/reference/Chat.md)
object to
[`shinychat::chat_ui()`](https://posit-dev.github.io/shinychat/r/reference/chat_ui.html)
and handle non-blocking asynchronous chat interactions automatically.

``` r
library(shiny)
library(shinychat)

ui <- bslib::page_fillable(
  chat_mod_ui("chat")
)

server <- function(input, output, session) {
  chat <- ellmer::chat_openai(
    system_prompt = "You're a trickster who answers in riddles",
    model = "gpt-4.1-nano"
  )

  chat_mod_server("chat", chat)
}

shinyApp(ui, server)
```

For fully custom streaming applications with a custom or no chat
interface, you can use
[`shinychat::markdown_stream()`](https://posit-dev.github.io/shinychat/r/reference/markdown_stream.html)
to stream responses into a Shiny app. This is particularly useful for
creating interactive chat applications where you want to display
responses as they are generated.

The following Shiny app demonstrates
[`markdown_stream()`](https://posit-dev.github.io/shinychat/r/reference/markdown_stream.html)
and uses both `$stream_async()` and `$chat_async()` to stream a story
from an OpenAI model. In the app, we ask the user for a prompt to
generate a story and then stream the story into the UI. Then we follow
up by asking the model for a story title and we use the response to
update the card title.

This example also highlights the difference between streaming and
non-streaming chat. Use `$stream_async()` with Shiny outputs that are
designed to work with generators, like
[`shinychat::markdown_stream()`](https://posit-dev.github.io/shinychat/r/reference/markdown_stream.html)
and
[`shinychat::chat_append()`](https://posit-dev.github.io/shinychat/r/reference/chat_append.html).
Use `$chat_async()` when you want the text response from the model, for
example the title of the story.

Also note that in most ellmer-powered Shiny apps, it’s best to wrap the
chat interaction in a
[`shiny::ExtendedTask`](https://rdrr.io/pkg/shiny/man/ExtendedTask.html)
to avoid blocking the rest of the app while the chat is being generated.
You can learn about `ExtendedTask` in Shiny’s [*Non-blocking operations*
article](https://shiny.posit.co/r/articles/improve/nonblocking/).

    library(shiny)
    library(bslib)
    library(ellmer)
    library(promises)
    library(shinychat)

    ui <- page_sidebar(
      title = "Interactive chat with async",
      sidebar = sidebar(
        textAreaInput("user_query", "Tell me a story about..."),
        input_task_button("ask_chat", label = "Generate a story")
      ),
      card(
        card_header(textOutput("story_title")),
        shinychat::output_markdown_stream("response"),
      )
    )

    server <- function(input, output) {
      chat_task <- ExtendedTask$new(function(user_query) {
        # We're using an Extended Task for chat completions to avoid blocking the
        # app. We also start the chat fresh each time, because the UI is not a
        # multi-turn conversation.
        chat <- chat_openai(
          system_prompt = "You are a rambling chatbot who likes to tell stories but gets distracted easily.",
          model = "gpt-4.1-nano"
        )

        # Stream the chat completion into the markdown stream. `markdown_stream()`
        # returns a promise onto which we'll chain the follow-up task of providing
        # a story title.
        stream <- chat$stream_async(user_query)
        stream_res <- shinychat::markdown_stream("response", stream)

        # Follow up by asking the LLM to provide a title for the story that we
        # return from the task.
        stream_res$then(function(value) {
          chat$chat_async(
            "What is the title of the story? Reply with only the title and nothing else."
          )
        })
      })

      bind_task_button(chat_task, "ask_chat")

      observeEvent(input$ask_chat, {
        chat_task$invoke(input$user_query)
      })

      observe({
        # Update the card title during generation and once complete
        switch(
          chat_task$status(),
          success = story_title(chat_task$result()),
          running = story_title("Generating your story..."),
          error = story_title("An error occurred while generating your story.")
        )
      })

      story_title <- reactiveVal("Your story will appear here!")
      output$story_title <- renderText(story_title())
    }

    shinyApp(ui = ui, server = server)

### Asynchronous streaming

For asynchronous streaming, you’d use the `stream()` method as before,
but the result is an [async
generator](https://coro.r-lib.org/reference/async_generator.html) from
the [coro package](https://coro.r-lib.org/). This is the same as a
regular [generator](https://coro.r-lib.org/articles/generator.html),
except that instead of giving you strings, it gives you promises that
resolve to strings.

``` r
stream <- chat$stream_async("What are some common uses of R?")
coro::async(function() {
  for (chunk in await_each(stream)) {
    cat(toupper(chunk))
  }
})()
#>  R IS COMMONLY USED FOR:
#>
#>  1. **STATISTICAL ANALYSIS**: PERFORMING VARIOUS STATISTICAL TESTS AND MODELS.
#>  2. **DATA VISUALIZATION**: CREATING PLOTS AND GRAPHS TO VISUALIZE DATA.
#>  3. **DATA MANIPULATION**: CLEANING AND TRANSFORMING DATA WITH PACKAGES LIKE DPLYR.
#>  4. **MACHINE LEARNING**: BUILDING PREDICTIVE MODELS AND ALGORITHMS.
#>  5. **BIOINFORMATICS**: ANALYZING BIOLOGICAL DATA, ESPECIALLY IN GENOMICS.
#>  6. **TIME SERIES ANALYSIS**: ANALYZING TEMPORAL DATA FOR TRENDS AND FORECASTS.
#>  7. **REPORT GENERATION**: CREATING DYNAMIC REPORTS WITH R MARKDOWN.
#>  8. **GEOSPATIAL ANALYSIS**: MAPPING AND ANALYZING GEOGRAPHIC DATA.
```

Async generators are very advanced and require a good understanding of
asynchronous programming in R. They are also the only way to present
streaming results in Shiny without blocking other users. Fortunately,
Shiny will soon have chat components that will make this easier, where
you’ll simply hand the result of `stream_async()` to a chat output.
