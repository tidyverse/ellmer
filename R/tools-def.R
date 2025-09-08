#' @include utils-S7.R
#' @include types.R
#' @include ellmer-package.R
NULL

#' Define a tool
#'
#' @description
#' Annotate a function for use in tool calls, by providing a name, description,
#' and type definition for the arguments.
#'
#' Learn more in `vignette("tool-calling")`.
#'
#' # ellmer 0.3.0
#'
#' In ellmer 0.3.0, the definition of the `tool()` function changed quite
#' a bit. To make it easier to update old versions, you can use an LLM with
#' the following system prompt
#'
#' ````
#' Help the user convert an ellmer 0.2.0 and earlier tool definition into a
#' ellmer 0.3.0 tool definition. Here's what changed:
#'
#' * All arguments, apart from the first, should be named, and the argument
#'   names no longer use `.` prefixes. The argument order should be function,
#'   name (as a string), description, then arguments, then anything
#'
#' * Previously `arguments` was passed as `...`, so all type specifications
#'   should now be moved into a named list and passed to the `arguments`
#'   argument. It can be omitted if the function has no arguments.
#'
#' ```R
#' # old
#' tool(
#'   add,
#'   "Add two numbers together"
#'   x = type_number(),
#'   y = type_number()
#' )
#'
#' # new
#' tool(
#'   add,
#'   name = "add",
#'   description = "Add two numbers together",
#'   arguments = list(
#'     x = type_number(),
#'     y = type_number()
#'   )
#' )
#' ```
#'
#' Don't respond; just let the user provide function calls to convert.
#' ````
#'
#' @param fun The function to be invoked when the tool is called. The return
#'   value of the function is sent back to the chatbot.
#'
#'   Expert users can customize the tool result by returning a
#'   [ContentToolResult] object.
#' @param name The name of the function. This can be omitted if `fun` is an
#'   existing function (i.e. not defined inline).
#' @param description A detailed description of what the function does.
#'   Generally, the more information that you can provide here, the better.
#' @param arguments A named list that defines the arguments accepted by the
#'   function. Each element should be created by a [`type_*()`][type_boolean]
#'   function (or `NULL` if you don't want the LLM to use that argument).
#' @param annotations Additional properties that describe the tool and its
#'   behavior. Usually created by [tool_annotations()], where you can find a
#'   description of the annotation properties recommended by the [Model Context
#'   Protocol](https://modelcontextprotocol.io/introduction).
#' @param convert Should JSON inputs be automatically convert to their
#'   R data type equivalents? Defaults to `TRUE`.
#' @param ... `r lifecycle::badge("deprecated")` Use `arguments` instead.
#' @param .name,.description,.convert,.annotations
#'   `r lifecycle::badge("deprecated")` Please switch to the non-prefixed
#'   equivalents.
#' @return An S7 `ToolDef` object.
#' @examples
#' \dontshow{ellmer:::vcr_example_start("tool")}
#' \dontshow{set.seed(1014)}
#' # First define the metadata that the model uses to figure out when to
#' # call the tool
#' tool_rnorm <- tool(
#'   rnorm,
#'   description = "Draw numbers from a random normal distribution",
#'   arguments = list(
#'     n = type_integer("The number of observations. Must be a positive integer."),
#'     mean = type_number("The mean value of the distribution."),
#'     sd = type_number("The standard deviation of the distribution. Must be a non-negative number.")
#'   )
#' )
#' tool_rnorm(n = 5, mean = 0, sd = 1)
#'
#' chat <- chat_openai()
#' # Then register it
#' chat$register_tool(tool_rnorm)
#'
#' # Then ask a question that needs it.
#' chat$chat("Give me five numbers from a random normal distribution.")
#'
#' # Look at the chat history to see how tool calling works:
#' chat
#' # Assistant sends a tool request which is evaluated locally and
#' # results are sent back in a tool result.
#'
#' \dontshow{ellmer:::vcr_example_end()}
#' @family tool calling helpers
#' @aliases ToolDef
#' @export
tool <- function(
  fun,
  description,
  ...,
  arguments = list(),
  name = NULL,
  convert = TRUE,
  annotations = list(),
  .name = deprecated(),
  .description = deprecated(),
  .convert = deprecated(),
  .annotations = deprecated()
) {
  if (!missing(...)) {
    lifecycle::deprecate_warn("0.3.0", "tool(...)", "tool(arguments)")
    arguments <- list(...)
  }
  if (lifecycle::is_present(.name)) {
    lifecycle::deprecate_warn("0.3.0", "tool(.name)", "tool(name)")
    name <- .name
  }
  if (lifecycle::is_present(.description)) {
    lifecycle::deprecate_warn(
      "0.3.0",
      "tool(.description)",
      "tool(description)"
    )
    description <- .description
  }
  if (lifecycle::is_present(.convert)) {
    lifecycle::deprecate_warn("0.3.0", "tool(.convert)", "tool(convert)")
    convert <- .convert
  }
  if (lifecycle::is_present(.annotations)) {
    lifecycle::deprecate_warn(
      "0.3.0",
      "tool(.annotations)",
      "tool(annotations)"
    )
    annotations <- .annotations
  }

  fun_expr <- enexpr(fun)
  check_function(fun)
  check_string(description)
  check_string(name, allow_null = TRUE)
  check_bool(convert)

  if (is.null(name)) {
    if (is.name(fun_expr)) {
      name <- as.character(fun_expr)
    } else {
      name <- unique_tool_name()
    }
  }
  if (!grepl("^[a-zA-Z0-9_-]+$", name)) {
    cli::cli_abort("{.arg name} must contain only letters, numbers, - and _.")
  }

  check_arguments(arguments, formals(fun))

  ToolDef(
    fun,
    name = name,
    description = description,
    arguments = TypeObject(properties = arguments),
    convert = convert,
    annotations = annotations
  )
}

ToolDef <- new_class(
  "ToolDef",
  parent = class_function,
  properties = list(
    name = prop_string(),
    description = prop_string(),
    arguments = TypeObject,
    convert = prop_bool(TRUE),
    annotations = class_list
  )
)

method(print, ToolDef) <- function(x, ...) {
  fake_call <- call2(x@name, !!!syms(names(x@arguments@properties)))

  cat_line("# <ellmer::ToolDef> ", deparse1(fake_call))
  cat_line("# @name: ", x@name)
  cat_line("# @description: ", x@description)
  cat_line("# @convert: ", x@convert)
  cat_line("#")
  print(S7_data(x))

  invisible(x)
}

check_arguments <- function(arguments, formals, call = caller_env()) {
  if (!is.list(arguments) || !(length(arguments) == 0 || is_named(arguments))) {
    stop_input_type(arguments, "a named list", call = call)
  }

  extra_args <- setdiff(names(arguments), names(formals))
  missing_args <- setdiff(names(formals), names(arguments))
  if (length(extra_args) > 0 || length(missing_args) > 0) {
    cli::cli_abort(
      c(
        "Names of {.arg arguments} must match formals of {.arg fun}",
        "*" = if (length(extra_args) > 0) {
          "Extra type definitions: {.val {extra_args}}"
        },
        "*" = if (length(missing_args) > 0) {
          "Missing type definitions: {.val {missing_args}}"
        }
      ),
      call = call
    )
  }

  for (nm in names(arguments)) {
    arg <- arguments[[nm]]
    if (!is.null(arg) && !S7_inherits(arg, Type)) {
      stop_input_type(
        arg,
        c("a <Type>", "NULL"),
        arg = paste0("arguments$", nm),
        call = call
      )
    }
  }

  invisible()
}

check_tool <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (!S7_inherits(x, ToolDef)) {
    stop_input_type(x, "a <ToolDef>", arg = arg, call = call)
  }
}

check_tools <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (!is_list(x)) {
    stop_input_type(x, "a list", arg = arg, call = call)
  }
  for (i in seq_along(x)) {
    check_tool(x[[i]], arg = paste0(arg, "[[", i, "]]"), call = call)
  }
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

#' Reject a tool call
#'
#' @description
#' Throws an error to reject a tool call. `tool_reject()` can be used within the
#' tool function to indicate that the tool call should not be processed.
#' `tool_reject()` can also be called in an `Chat$on_tool_request()` callback.
#'  When used in the callback, the tool call is rejected before the tool
#' function is invoked.
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
#'   if (isTRUE(allow_read)) {
#'     dir(pattern = "[.](r|R|csv)$")
#'   } else {
#'     tool_reject()
#'   }
#' }
#'
#' chat$register_tool(tool(
#'   list_files,
#'   "List files in the user's current directory"
#' ))
#'
#' chat$chat("What files are available in my current directory?")
#' #> [tool call] list_files()
#' #> Would you like to allow access to your current directory? (Yes/no/cancel) no
#' #> #> Error: Tool call rejected. The user has chosen to disallow the tool #' call.
#' #> It seems I am unable to access the files in your current directory right now.
#' #> If you can tell me what specific files you're looking for or if you can #' provide
#' #> the list, I can assist you further.
#'
#' chat$chat("Try again.")
#' #> [tool call] list_files()
#' #> Would you like to allow access to your current directory? (Yes/no/cancel) yes
#' #> #> app.R
#' #> #> data.csv
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
#'   function() dir(pattern = "[.](r|R|csv)$"),
#'   "List files in the user's current directory"
#' )
#'
#' chat <- chat_openai(model = "gpt-4.1-nano")
#' chat$register_tool(packaged_list_files_tool)
#'
#' always_allowed <- c()
#'
#' # ContentToolRequest
#' chat$on_tool_request(function(request) {
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
#' @family tool calling helpers
#' @export
tool_reject <- function(
  reason = "The user has chosen to disallow the tool call."
) {
  check_string(reason)

  abort(
    paste("Tool call rejected.", reason),
    class = "ellmer_tool_reject"
  )
}


unique_tool_name <- function() {
  the$cur_tool_id <- (the$cur_tool_id %||% 0) + 1
  sprintf("tool_%03d", the$cur_tool_id)
}
