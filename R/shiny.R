#' Open a live chat application
#'
#' @description
#'
#' * `live_console()` lets you chat interactively in the console.
#' * `live_browser()` lets you chat interactively in a browser.
#'
#' Note that these functions will mutate the input `chat` object as
#' you chat because your turns will be appended to the history.
#'
#' @param chat A chat object created by [chat_openai()] or friends.
#' @param quiet If `TRUE`, suppresses the initial message that explains how
#'   to use the console.
#' @export
#' @returns (Invisibly) The input `chat`.
#' @examples
#' \dontrun{
#' chat <- chat_anthropic()
#' live_console(chat)
#' live_browser(chat)
#' }
live_console <- function(chat, quiet = FALSE) {
  if (!is_interactive()) {
    cli::cli_abort("The chat console is only available in interactive mode.")
  }

  if (!isTRUE(quiet)) {
    cli::cat_boxx(
      c(
        "Entering chat console.",
        "Use \"\"\" for multi-line input.",
        "Type 'Q' to quit."
      ),
      padding = c(0, 1, 0, 1),
      border_style = "double"
    )
  }

  repeat {
    user_input <- readline(prompt = ">>> ")

    if (user_input == "Q") {
      break
    }

    if (!grepl("\\S", user_input)) {
      next
    }

    if (grepl('^\\s*"""', user_input)) {
      repeat {
        next_input <- readline(prompt = '... ')
        user_input <- paste0(user_input, "\n", next_input)
        if (grepl('"""\\s*$', next_input)) {
          break
        }
      }
      # Strip leading and trailing """, using regex
      user_input <- gsub('^\\s*"""\\s*', '', user_input)
      user_input <- gsub('\\s*"""\\s*$', '', user_input)
    }

    # Process the input using the provided LLM function
    chat$chat(user_input, echo = TRUE)
    cat("\n")
  }

  invisible(chat)
}

#' @export
#' @rdname live_console
live_browser <- function(chat, quiet = FALSE) {
  check_installed("shiny")
  check_installed("shinychat", version = "0.1.1.9001")

  if (!isTRUE(quiet)) {
    cli::cat_boxx(
      c("Entering interactive chat", "Press Ctrl+C to quit."),
      padding = c(0, 1, 0, 1),
      border_style = "double"
    )
  }

  tryCatch(
    shiny::runGadget(shinychat::chat_app(chat, options = list(quiet = TRUE))),
    interrupt = function(cnd) NULL
  )
  invisible(chat)
}
