#' Create metadata for a tool
#'
#' @description
#' In order to use a function as a tool in a chat, you need to craft the right
#' call to [tool()]. This function helps you do that for documented functions by
#' extracting the function's R documentation and using an LLM to generate the
#' `tool()` call. It's meant to be used interactively while writing your
#' code, not as part of your final code.
#'
#' If the function has package documentation, that will be used. Otherwise, if
#' the source code of the function can be automatically detected, then the
#' comments immediately preceding the function are used (especially helpful if
#' those are roxygen2 comments). If neither are available, then just the function
#' signature is used.
#'
#' Note that this function is inherently imperfect. It can't handle all possible
#' R functions, because not all parameters are suitable for use in a tool call
#' (for example, because they're not serializable to simple JSON objects). The
#' documentation might not specify the expected shape of arguments to the level
#' of detail that would allow an exact JSON schema to be generated. Please be
#' sure to review the generated code before using it!
#'
#' @param topic A symbol or string literal naming the function to create
#'   metadata for. Can also be an expression of the form `pkg::fun`.
#' @param chat A `Chat` object used to generate the output. If `NULL`
#'   (the default) uses [chat_openai()].
#' @param model `lifecycle::badge("deprecated")` Formally used for definining
#'   the model used by the chat. Now supply `chat` instead.
#' @param echo Emit the registration code to the console. Defaults to `TRUE` in
#'   interactive sessions.
#' @param verbose If `TRUE`, print the input we send to the LLM, which may be
#'   useful for debugging unexpectedly poor results.
#'
#' @return A `register_tool` call that you can copy and paste into your code.
#'   Returned invisibly if `echo` is `TRUE`.
#'
#' @examples
#' \dontrun{
#'   # These are all equivalent
#'   create_tool_def(rnorm)
#'   create_tool_def(stats::rnorm)
#'   create_tool_def("rnorm")
#'   create_tool_def("rnorm", chat = chat_azure_openai())
#' }
#'
#' @export
create_tool_def <- function(
  topic,
  chat = NULL,
  model = deprecated(),
  echo = interactive(),
  verbose = FALSE
) {
  expr <- enexpr(topic)

  check_exclusive(model, chat, .require = FALSE)
  if (lifecycle::is_present(model)) {
    lifecycle::deprecate_warn(
      when = "1.0.0",
      what = "create_tool_def(model)",
      with = "create_tool_def(chat)"
    )
    chat <- chat_openai(model = model)
  }
  if (is.null(chat)) {
    chat <- chat_openai()
  } else if (is_chat(chat)) {
    chat <- chat$clone()
  } else {
    stop_input_type(chat, "a <Chat> object", allow_null = TRUE)
  }
  check_echo(echo)
  check_bool(verbose)

  tool_prompt <- read_file(system.file("tool_prompt.md", package = "ellmer"))
  chat$set_system_prompt(tool_prompt)

  pkg <- NULL
  fun <- format(expr)

  # Ensure `expr` is a string literal, a symbol, or an expression of the form
  # `pkg::fun` or `pkg:::fun`.
  if (is_call(expr)) {
    if (
      !identical(expr[[1]], quote(`::`)) ||
        !is_symbol(expr[[2]]) ||
        !is_symbol(expr[[3]])
    ) {
      cli::cli_abort(
        "Expected a symbol or a string literal, or an expression of the form `pkg::fun` or `pkg:::fun`."
      )
    }
    pkg <- as.character(expr[[2]])
    fun <- as.character(expr[[3]])
  } else if (is_symbol(expr)) {
    fun <- as.character(expr)
  } else if (!is_string(expr)) {
    cli::cli_abort(
      "Expected a symbol or a string literal, or an expression of the form `pkg::fun` or `pkg:::fun`."
    )
  }

  help_text <- get_help_text(fun, pkg)
  if (is.null(help_text)) {
    # Package help documentation wasn't found; try comments instead
    help_text <- paste(extract_comments_and_signature(topic), collapse = "\n")
  }

  topic_str <- format(expr)

  payload <- paste_c(
    c("<name>", topic_str, "</name>\n\n"),
    c("<documentation>\n", help_text, "</documentation>\n\n")
  )

  if (isTRUE(verbose)) {
    cli::cli_rule(cli::style_bold("Prompt"))
    message(payload, "\n\n")
    cli::cli_rule(cli::style_bold("Response"))
  }

  chat$chat(payload, echo = echo)
}

help_to_text <- function(help_files) {
  file_contents <- NULL
  fake_pager <- function(files, header, title, delete.file) {
    if (delete.file) {
      on.exit(unlink(files))
    }

    for (file in files) {
      file_contents <<- c(
        file_contents,
        readLines(files, warn = FALSE),
        "\n"
      )
    }
  }

  op <- options(pager = fake_pager)
  on.exit(options(op), add = TRUE)
  rd_opts <- tools::Rd2txt_options(underline_titles = FALSE)
  on.exit(tools::Rd2txt_options(rd_opts), add = TRUE)

  print(help_files)

  paste(file_contents, collapse = "\n")
}

get_help_text <- function(topic, package = NULL) {
  # The extra parens around topic and package are to ensure that they're
  # evaluated; see the last example on ?utils::help.
  help_files <- utils::help((topic), package = (package), help_type = "text")
  if (length(help_files) == 0) {
    return(NULL)
  } else {
    help_to_text(help_files)
  }
}

# If the function source cannot be found, at least provide the function
# signature
get_signature <- function(func) {
  args <- utils::capture.output(args(func))
  paste0(paste0(args[-length(args)], collapse = "\n"), " ...")
}

is_file_accessible <- function(filename) {
  !is.null(filename) && file.access(filename, 4) == 0
}

find_comments_start <- function(lines, start_line) {
  while (start_line > 1 && grepl("^\\s*#", lines[start_line - 1])) {
    start_line <- start_line - 1
  }
  start_line
}

extract_comments <- function(filename, start_line) {
  lines <- readLines(filename)
  comments_start <- find_comments_start(lines, start_line)
  if (comments_start == start_line) {
    return(NULL)
  }
  comments_lines <- lines[comments_start:(start_line - 1)]
  paste(comments_lines, collapse = "\n")
}

# Given a function, extract the comments (if any) and function signature. Relies
# on srcref information, so functions entered at the repl will be missing
# documentation.
extract_comments_and_signature <- function(func) {
  src <- attr(func, "srcref")
  sig <- get_signature(func)

  if (is.null(src)) {
    return(sig)
  }

  filename <- attr(src, "srcfile")$filename
  if (!is_file_accessible(filename)) {
    return(sig)
  }

  start_line <- src[1]
  comments <- extract_comments(filename, start_line)
  if (is.null(comments)) {
    return(sig)
  }

  paste0(comments, "\n", sig)
}
