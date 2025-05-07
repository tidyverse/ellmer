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
#'   sd = type_number("The standard deviation of the distribution. Must be a non-negative number."),
#'   .annotations = tool_annotations(
#'     title = "Draw Random Normal Numbers",
#'     read_only_hint = TRUE,
#'     open_world_hint = FALSE
#'   )
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
#' @family tool calling helpers
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
#' Tool annotations are additional properties that, when passed to the
#' `.annotations` argument of [tool()], provide additional information about the
#' tool and its behavior. This information can be used for display to users, for
#' example in a Shiny app or another user interface.
#'
#' The annotations in `tool_annotations()` are drawn from the [Model Context
#' Protocol](https://modelcontextprotocol.io/introduction) and are considered
#' *hints*. Tool authors should use these annotations to communicate tool
#' properties, but users should note that these annotations are not guaranteed.
#'
#' @examples
#' # See ?tool() for a full example using this function.
#' # We're creating a tool around R's `rnorm()` function to allow the chatbot to
#' # generate random numbers from a normal distribution.
#' tool_rnorm <- tool(
#'   rnorm,
#'   # Describe the tool function to the LLM
#'   .description = "Drawn numbers from a random normal distribution",
#'   # Describe the parameters used by the tool function
#'   n = type_integer("The number of observations. Must be a positive integer."),
#'   mean = type_number("The mean value of the distribution."),
#'   sd = type_number("The standard deviation of the distribution. Must be a non-negative number."),
#'   # Tool annotations optionally provide additional context to the LLM
#'   .annotations = tool_annotations(
#'     title = "Draw Random Normal Numbers",
#'     read_only_hint = TRUE, # the tool does not modify any state
#'     open_world_hint = FALSE # the tool does not interact with the outside world
#'   )
#' )
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
#' @family tool calling helpers
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
