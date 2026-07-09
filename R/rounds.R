#' @include turns.R
NULL

#' A round of conversation
#'
#' @description
#' A `Round` groups one real user [Turn] with the assistant and tool-result
#' [Turn]s that follow it, i.e. everything that happens in response to one user
#' message, including any tool-calling loop. `Round`s are derived from a
#' `Chat`'s flat turn history on read (e.g. via `chat$get_rounds()`); they
#' aren't stored separately.
#'
#' @param input A list of [Turn]s that begin the round: the real user turn,
#'   optionally preceded by one or more system turns. The user turn is the last
#'   element (a round consisting solely of system turns is possible, e.g. a
#'   chat that only has a system prompt so far).
#' @param response A list of [Turn]s (assistant and tool-result) that
#'   followed `input`.
#' @param complete Whether the round is complete, i.e. `response` is non-empty
#'   and its last element is a finished (non-partial) assistant turn with no
#'   pending tool request.
#' @export
#' @return An S7 `Round` object
Round <- new_class(
  "Round",
  properties = list(
    input = prop_list_of(Turn),
    response = prop_list_of(Turn),
    complete = new_property(
      class = class_logical,
      getter = function(self) {
        n <- length(self@response)
        n > 0 &&
          is_assistant_turn(self@response[[n]]) &&
          !is_partial_turn(self@response[[n]]) &&
          !turn_has_tool_request(self@response[[n]])
      }
    )
  ),
  validator = function(self) {
    if (length(self@input) == 0) {
      "`input` must contain at least one turn."
    } else if (some(self@input, is_tool_result_turn)) {
      "`input` must not contain tool-result turns."
    }
  }
)

get_rounds <- function(turns) {
  rounds <- list()
  current <- NULL

  for (turn in turns) {
    is_input <- is_system_turn(turn) ||
      (is_user_turn(turn) && !is_tool_result_turn(turn))

    if (is_input) {
      if (!is.null(current) && length(current@response) > 0) {
        rounds[[length(rounds) + 1]] <- current
        current <- NULL
      }
      if (is.null(current)) {
        current <- Round(input = list(turn), response = list())
      } else {
        current@input[[length(current@input) + 1]] <- turn
      }
    } else if (is.null(current)) {
      cli::cli_abort(
        "Found a response turn with no preceding input turn to start a round."
      )
    } else {
      current@response[[length(current@response) + 1]] <- turn
    }
  }
  if (!is.null(current)) {
    rounds[[length(rounds) + 1]] <- current
  }

  rounds
}

method(format, Round) <- function(x, ...) {
  turns <- c(x@input, x@response)
  paste0(map_chr(turns, format, ...), collapse = "")
}

method(print, Round) <- function(x, ...) {
  cat("<Round>\n")
  for (turn in c(x@input, x@response)) {
    print(turn, ...)
  }
  invisible(x)
}

method(contents_text, Round) <- function(content) {
  turns <- c(content@input, content@response)
  res <- map_chr(turns, function(turn) {
    paste0("<", turn@role, ">\n", contents_text(turn), "\n</", turn@role, ">")
  })
  paste(res, collapse = "\n\n")
}

method(contents_html, Round) <- function(content) {
  turns <- c(content@input, content@response)
  res <- map_chr(turns, function(turn) {
    paste0("<h2>", turn_role_title(turn), "</h2>\n", contents_html(turn))
  })
  paste(res, collapse = "\n")
}

method(contents_markdown, Round) <- function(content, heading_level = 2) {
  turns_markdown(c(content@input, content@response), heading_level)
}
