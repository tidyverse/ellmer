# Create a stream controller

Creates a controller that can cancel an in-progress stream. Pass it to
[Chat](https://ellmer.tidyverse.org/dev/reference/Chat.md)'s `$stream()`
or `$stream_async()` via the `controller` argument, then call
`$cancel()` from anywhere (e.g. a Shiny observer) to stop the stream
after the next chunk arrives.

The same controller can be reused across multiple streams. Call
`$reset()` to clear the cancelled state, or pass it directly to a new
`$stream()` call — it will be reset automatically.

## Usage

``` r
stream_controller()
```

## Value

An `ellmer_stream_controller` object with the following elements:

- `$cancel(reason = "cancelled")`: Cancel the stream. The `reason`
  string is stored on the controller and used as the
  [AssistantPartialTurn](https://ellmer.tidyverse.org/dev/reference/Turn.md)'s
  `reason` property.

- `$reset()`: Clear the cancelled state and reason.

- `$cancelled`: A logical flag indicating whether the controller has
  been cancelled.

- `$reason`: The cancellation reason string, or `NULL` if not cancelled.

## Async cancellation in Shiny

In a Shiny app, use an
[ExtendedTask](https://rdrr.io/pkg/shiny/man/ExtendedTask.html) for
non-blocking chat and a `stream_controller()` to wire up a cancel
button:

    controller <- stream_controller()

    chat_task <- ExtendedTask$new(function(user_query, controller = NULL) {
      chat <- chat_openai(model = "gpt-4.1-nano")
      stream <- chat$stream_async(user_query, controller = controller)
      shinychat::markdown_stream("response", stream)
    })

    observeEvent(input$ask, {
      controller <<- stream_controller()
      chat_task$invoke(input$query, controller = controller)
    })

    observeEvent(input$cancel, {
      controller$cancel()
    })

## Examples

``` r
if (FALSE) { # rlang::is_interactive()
chat <- chat_openai(model = "gpt-5.4-nano")

ctrl <- stream_controller()
stream <- chat$stream("Write a short story.", controller = ctrl)

i <- 0
coro::loop(for (chunk in stream) {
  i <- i + 1
  if (i > 10) ctrl$cancel()
})

chat
}
```
