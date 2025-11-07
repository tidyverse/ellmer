# Reject a tool call

Throws an error to reject a tool call. `tool_reject()` can be used
within the tool function to indicate that the tool call should not be
processed. `tool_reject()` can also be called in an
`Chat$on_tool_request()` callback. When used in the callback, the tool
call is rejected before the tool function is invoked.

Here's an example where
[`utils::askYesNo()`](https://rdrr.io/r/utils/askYesNo.html) is used to
ask the user for permission before accessing their current working
directory. This happens directly in the tool function and is appropriate
when you write the tool definition and know exactly how it will be
called.

    chat <- chat_openai(model = "gpt-4.1-nano")

    list_files <- function() {
      allow_read <- utils::askYesNo(
        "Would you like to allow access to your current directory?"
      )
      if (isTRUE(allow_read)) {
        dir(pattern = "[.](r|R|csv)$")
      } else {
        tool_reject()
      }
    }

    chat$register_tool(tool(
      list_files,
      "List files in the user's current directory"
    ))

    chat$chat("What files are available in my current directory?")
    #> [tool call] list_files()
    #> Would you like to allow access to your current directory? (Yes/no/cancel) no
    #> #> Error: Tool call rejected. The user has chosen to disallow the tool #' call.
    #> It seems I am unable to access the files in your current directory right now.
    #> If you can tell me what specific files you're looking for or if you can #' provide
    #> the list, I can assist you further.

    chat$chat("Try again.")
    #> [tool call] list_files()
    #> Would you like to allow access to your current directory? (Yes/no/cancel) yes
    #> #> app.R
    #> #> data.csv
    #> The files available in your current directory are "app.R" and "data.csv".

You can achieve a similar experience with tools written by others by
using a `tool_request` callback. In the next example, imagine the tool
is provided by a third-party package. This example implements a simple
menu to ask the user for consent before running *any* tool.

    packaged_list_files_tool <- tool(
      function() dir(pattern = "[.](r|R|csv)$"),
      "List files in the user's current directory"
    )

    chat <- chat_openai(model = "gpt-4.1-nano")
    chat$register_tool(packaged_list_files_tool)

    always_allowed <- c()

    # ContentToolRequest
    chat$on_tool_request(function(request) {
      if (request@name %in% always_allowed) return()

      answer <- utils::menu(
        title = sprintf("Allow tool `%s()` to run?", request@name),
        choices = c("Always", "Once", "No"),
        graphics = FALSE
      )

      if (answer == 1) {
        always_allowed <<- append(always_allowed, request@name)
      } else if (answer %in% c(0, 3)) {
        tool_reject()
      }
    })

    # Try choosing different answers to the menu each time
    chat$chat("What files are available in my current directory?")
    chat$chat("How about now?")
    chat$chat("And again now?")

## Usage

``` r
tool_reject(reason = "The user has chosen to disallow the tool call.")
```

## Arguments

- reason:

  A character string describing the reason for rejecting the tool call.

## Value

Throws an error of class `ellmer_tool_reject` with the provided reason.

## See also

Other tool calling helpers:
[`tool()`](https://ellmer.tidyverse.org/dev/reference/tool.md),
[`tool_annotations()`](https://ellmer.tidyverse.org/dev/reference/tool_annotations.md)
