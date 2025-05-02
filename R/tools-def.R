#' @include utils-S7.R
#' @include types.R
NULL

#' Define a tool
#'
#' @description
#' Define an R function for use by a chatbot. The function will always be
#' run in the current R instance.
#'
#' Learn more in `vignette("tool-calling")`.
#'
#' @param .fun The function to be invoked when the tool is called. The return
#'   value of the function is sent back to the chatbot.
#'
#'   Expert users can customize the tool result by returning a
#'   [ContentToolResult] object.
#' @param .name The name of the function.
#' @param .description A detailed description of what the function does.
#'   Generally, the more information that you can provide here, the better.
#' @param .annotations Additional properties that describe the tool and its
#'   behavior. Usually created by [tool_annotations()], where you can find a
#'   description of the annotation properties recommended by the [Model Context
#'   Protocol](https://modelcontextprotocol.io/introduction).
#' @param ... Name-type pairs that define the arguments accepted by the
#'   function. Each element should be created by a [`type_*()`][type_boolean]
#'   function.
#' @return An S7 `ToolDef` object.
#' @examplesIf has_credentials("openai")
#'
#' # First define the metadata that the model uses to figure out when to
#' # call the tool
#' tool_rnorm <- tool(
#'   rnorm,
#'   "Drawn numbers from a random normal distribution",
#'   n = type_integer("The number of observations. Must be a positive integer."),
#'   mean = type_number("The mean value of the distribution."),
#'   sd = type_number("The standard deviation of the distribution. Must be a non-negative number.")
#' )
#' chat <- chat_openai()
#' # Then register it
#' chat$register_tool(tool_rnorm)
#'
#' # Then ask a question that needs it.
#' chat$chat("
#'   Give me five numbers from a random normal distribution.
#' ")
#'
#' # Look at the chat history to see how tool calling works:
#' # Assistant sends a tool request which is evaluated locally and
#' # results are send back in a tool result.
#'
#' @family Tool calling
#' @export
tool <- function(.fun, .description, ..., .name = NULL, .annotations = list()) {
  if (is.null(.name)) {
    fun_expr <- enexpr(.fun)
    if (is.name(fun_expr)) {
      .name <- as.character(fun_expr)
    } else {
      .name <- unique_tool_name()
    }
  }
  ToolDef(
    fun = .fun,
    name = .name,
    description = .description,
    arguments = type_object(...),
    annotations = .annotations
  )
}

#' Tool annotations
#'
#' @description
#' Tool annotations are additional properties that can be used to describe a
#' tool to clients, for example a Shiny app or another user interface.
#'
#' The annotations in `tool_annotations()` are drawn from the [Model Context
#' Protocol](https://modelcontextprotocol.io/introduction) and are considered
#' *hints*. Tool authors should use these annotations to communicate tool
#' properties, but users should note that these annotations are not guaranteed.
#'
#' @param title A human-readable title for the tool.
#' @param read_only_hint If `TRUE`, the tool does not modify its environment.
#' @param open_world_hint If `TRUE`, the tool may interact with an "open world"
#'   of external entities. If `FALSE`, the tool's domain of interaction is
#'   closed. For example, the world of a web search tool is open, but the world
#'   of a memory tool is not.
#' @param idempotent_hint If `TRUE`, calling the tool repeatedly with the same
#'   arguments will have no additional effect on its environment. (Only
#'   meaningful when `read_only_hint` is `FALSE`.)
#' @param destructive_hint If `TRUE`, the tool may perform destructive updates
#'   to its environment, otherwise it only performs additive updates. (Only
#'   meaningful when `read_only_hint` is `FALSE`.)
#' @param ... Additional named parameters to include in the tool annotations.
#'
#' @return A list of tool annotations.
#'
#' @family Tool calling
#' @export
tool_annotations <- function(
  title = NULL,
  read_only_hint = NULL,
  open_world_hint = NULL,
  idempotent_hint = NULL,
  destructive_hint = NULL,
  ...
) {
  # Snake-cased names and descriptions from the MCP 2025-03-26 Schema
  # https://github.com/modelcontextprotocol/specification/blob/72516795/schema/2025-03-26/schema.json#L2050-L2074
  check_character(title, allow_null = TRUE)
  check_bool(read_only_hint, allow_null = TRUE)
  check_bool(open_world_hint, allow_null = TRUE)
  check_bool(idempotent_hint, allow_null = TRUE)
  check_bool(destructive_hint, allow_null = TRUE)

  compact(list2(
    title = title,
    read_only_hint = read_only_hint,
    open_world_hint = open_world_hint,
    idempotent_hint = idempotent_hint,
    destructive_hint = destructive_hint,
    ...
  ))
}

#' Reject a tool call
#'
#' @description
#' Throws an error to reject a tool call. `tool_reject()` can be used within the
#' tool function to indicate that the tool call should not be processed, or it
#' can be used in a `tool_request` callback registered with a `Chat` object
#' using the `Chat$register_callback()` method. When used in the callback, the
#' tool call is rejected before the tool function is invoked.
#'
#' Here's an example where `utils::askYesNo()` is used to ask the user for
#' permission before accessing their current working directory. This happens
#' directly in the tool function and is appropriate when you write the tool
#' definition and know exactly how it will be called.
#'
#' ```r
#' chat <- chat_openai(model = "gpt-4.1-nano")
#'
#' list_files <- function() {
#'   allow_read <- utils::askYesNo(
#'     "Would you like to allow access to your current directory?"
#'   )
#'   if (!isTRUE(allow_read)) tool_reject()
#'   c("app.R", "data.csv")
#' }
#'
#' chat$register_tool(tool(
#'   list_files,
#'   "List files in the user's current directory"
#' ))
#'
#' chat$chat("What files are available in my current directory?")
#' #> ◯ [tool call] list_files()
#' #> Would you like to allow access to your current directory? (Yes/no/cancel) no
#' #> ■ #> Error: Tool call rejected. The user has chosen to disallow the tool #' call.
#' #> It seems I am unable to access the files in your current directory right now.
#' #> If you can tell me what specific files you're looking for or if you can #' provide
#' #> the list, I can assist you further.
#'
#' chat$chat("Try again.")
#' #> ◯ [tool call] list_files()
#' #> Would you like to allow access to your current directory? (Yes/no/cancel) yes
#' #> ● #> app.R
#' #>   #> data.csv
#' #> The files available in your current directory are "app.R" and "data.csv".
#' ```
#'
#' You can achieve a similar experience with tools written by others by using a
#' `tool_request` callback. In the next example, imagine the tool is provided by
#' a third-party package. This example implements a simple menu to ask the user
#' for consent before running *any*  tool.
#'
#' ```r
#' packaged_list_files_tool <- tool(
#'   function() c("app.R", "data.csv"),
#'   "List files in the user's current directory"
#' )
#'
#' chat <- chat_openai(model = "gpt-4.1-nano")
#' chat$register_tool(packaged_list_files_tool)
#'
#' always_allowed <- c()
#'
#' # ContentToolRequest
#' chat$register_callback("tool_request", function(request) {
#'   if (request@name %in% always_allowed) return()
#'
#'   answer <- utils::menu(
#'     title = sprintf("Allow tool `%s()` to run?", request@name),
#'     choices = c("Always", "Once", "No"),
#'     graphics = FALSE
#'   )
#'
#'   if (answer == 1) {
#'     always_allowed <<- append(always_allowed, request@name)
#'   } else if (answer %in% c(0, 3)) {
#'     tool_reject()
#'   }
#' })
#'
#' # Try choosing different answers to the menu each time
#' chat$chat("What files are available in my current directory?")
#' chat$chat("How about now?")
#' chat$chat("And again now?")
#' ```
#'
#' @param reason A character string describing the reason for rejecting the
#'   tool call.
#' @return Throws an error of class `ellmer_tool_reject` with the provided
#'   reason.
#'
#' @family Tool calling
#' @export
tool_reject <- function(
  reason = "The user has chosen to disallow the tool call."
) {
  check_character(reason)

  rlang::abort(
    paste("Tool call rejected.", reason),
    class = "ellmer_tool_reject"
  )
}

ToolDef <- new_class(
  "ToolDef",
  properties = list(
    name = prop_string(),
    fun = class_function,
    description = prop_string(),
    arguments = TypeObject,
    annotations = class_list
  )
)
unique_tool_name <- function() {
  the$cur_tool_id <- (the$cur_tool_id %||% 0) + 1
  sprintf("tool_%03d", the$cur_tool_id)
}
