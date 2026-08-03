#' @include turns.R
NULL

#' A round of conversation
#'
#' @description
#' A `Round` groups a user [Turn] with the assistant and tool-result [Turn]s
#' that follow it, i.e. everything that happens in response to one user message,
#' including any tool-calling loop. `Round`s are an alternative view of a
#' [Chat]'s flat turn history.
#'
#' `Round`s also expose a read-only `@complete` property: `TRUE` if `response`
#' is non-empty and its last element is a finished (non-partial) assistant
#' turn with no pending tool request, `FALSE` otherwise.
#'
#' @param input A list of the input-side [Turn]s that begin the round: the
#'   turns between the end of the previous round and the user turn that
#'   triggered this one. In the common case this is a length-1 list holding just
#'   that user turn, but it can also be preceded by system turns.
#'
#'   Because [Chat]'s `$set_turns()` accepts an arbitrary list of turns, `input`
#'   may in principle hold multiple user turns or system turns in other
#'   positions; a round consisting solely of system turns is also possible (e.g.
#'   a chat that so far only has a system prompt).
#' @param response A list of [Turn]s (assistant and tool-result) that
#'   followed `input`.
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
  if (length(turns) == 0) {
    return(list())
  }

  is_input <- map_lgl(turns, is_input_turn)
  if (!is_input[[1]]) {
    cli::cli_abort(
      "Found a response turn with no preceding input turn to start a round."
    )
  }

  # A new round starts at the first input turn of each run of input turns, so
  # consecutive input turns (e.g. a system turn followed by a user turn) are
  # gathered into the `input` of a single round.
  prev_is_input <- c(FALSE, is_input[-length(is_input)])
  round_id <- cumsum(is_input & !prev_is_input)

  rounds <- map2(
    split(turns, round_id),
    split(is_input, round_id),
    function(turns, is_input) {
      Round(input = turns[is_input], response = turns[!is_input])
    }
  )
  unname(rounds)
}

is_input_turn <- function(x) {
  is_system_turn(x) || (is_user_turn(x) && !is_tool_result_turn(x))
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
