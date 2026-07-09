#' @include turns.R
NULL

#' A round of conversation
#'
#' @description
#' A `Round` groups a single real user [Turn] with the assistant and
#' tool-result [Turn]s that follow it, i.e. everything that happens in
#' response to one user message, including any tool-calling loop. `Round`s
#' are derived from a `Chat`'s flat turn history on read (e.g. via
#' `chat$get_rounds()`); they aren't stored separately.
#'
#' @param input The [UserTurn] that started the round.
#' @param response A list of [Turn]s (assistant and tool-result) that
#'   followed `input`.
#' @param complete Whether the round is complete, i.e. `response` is
#'   non-empty and its last element is an assistant turn with no pending
#'   tool request.
#' @export
#' @return An S7 `Round` object
Round <- new_class(
  "Round",
  properties = list(
    input = UserTurn,
    response = prop_list_of(Turn),
    complete = new_property(
      class = class_logical,
      getter = function(self) {
        n <- length(self@response)
        n > 0 &&
          is_assistant_turn(self@response[[n]]) &&
          !turn_has_tool_request(self@response[[n]])
      }
    )
  ),
  validator = function(self) {
    if (is_tool_result_turn(self@input)) {
      "`input` must be a user turn that is not a tool result."
    }
  }
)

get_rounds <- function(turns) {
  rounds <- list()
  current <- NULL

  for (turn in turns) {
    if (is_system_turn(turn)) {
      rounds[[length(rounds) + 1]] <- turn
    } else if (is_user_turn(turn) && !is_tool_result_turn(turn)) {
      if (!is.null(current)) {
        rounds[[length(rounds) + 1]] <- current
      }
      current <- Round(input = turn, response = list())
    } else if (is.null(current)) {
      cli::cli_abort(
        "Found a tool-result turn with no preceding user turn to start a round."
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
  contents <- c(format(x@input), map_chr(x@response, format, ...))
  paste0(contents, collapse = "")
}

method(print, Round) <- function(x, ...) {
  cat("<Round>\n")
  cat(format(x))
  invisible(x)
}
