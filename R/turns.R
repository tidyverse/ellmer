#' @include utils-S7.R
NULL

#' A user, assistant, or system turn
#'
#' @description
#' Every conversation with a chatbot consists of pairs of user and assistant
#' turns, corresponding to an HTTP request and response. These turns are
#' represented by the `Turn` object, which contains a list of [Content]s representing
#' the individual messages within the turn. These might be text, images, tool
#' requests (assistant only), or tool responses (user only).
#'
#' `UserTurn`, `AssistantTurn`, and `SystemTurn` are specialized subclasses
#' of `Turn` for different types of conversation turns. `AssistantTurn` includes
#' additional metadata about the API response.
#'
#' Note that a call to `$chat()` and related functions may result in multiple
#' user-assistant turn cycles. For example, if you have registered tools,
#' ellmer will automatically handle the tool calling loop, which may result in
#' any number of additional cycles. Learn more about tool calling in
#' `vignette("tool-calling")`.
#'
#' @param contents A list of [Content] objects.
#' @export
#' @return An S7 `Turn` object
#' @examples
#' UserTurn(list(ContentText("Hello, world!")))
Turn <- new_class(
  "Turn",
  properties = list(
    contents = prop_list_of(Content),
    text = new_property(
      class = class_character,
      getter = function(self) contents_text(self)
    ),
    role = new_property(
      class = class_character,
      getter = function(self) "unknown"
    )
  ),
  constructor = function(contents = list()) {
    if (is.character(contents)) {
      contents <- list(ContentText(paste0(contents, collapse = "\n")))
    }
    new_object(S7_object(), contents = contents)
  }
)

#' @rdname Turn
#' @export
UserTurn <- new_class(
  "UserTurn",
  parent = Turn,
  properties = list(
    role = new_property(
      class = class_character,
      getter = function(self) "user"
    )
  )
)

#' @rdname Turn
#' @export
SystemTurn <- new_class(
  "SystemTurn",
  parent = Turn,
  properties = list(
    role = new_property(
      class = class_character,
      getter = function(self) "system"
    )
  )
)

#' @param json The serialized JSON corresponding to the underlying data of
#'   the turns. This is useful if there's information returned by the provider
#'   that ellmer doesn't otherwise expose.
#' @param tokens A numeric vector of length 3 representing the number of
#'   input tokens (uncached), output tokens, and input tokens (cached)
#'   used in this turn.
#' @param cost The cost of the turn in dollars.
#' @param duration The duration of the request in seconds.
#' @export
#' @rdname Turn
#' @return An S7 `AssistantTurn` object
AssistantTurn <- new_class(
  "AssistantTurn",
  parent = Turn,
  properties = list(
    json = class_list,
    tokens = new_property(
      class_numeric,
      default = c(NA_real_, NA_real_, NA_real_),
      validator = function(value) {
        if (length(value) != 3) {
          "must be length three"
        }
      }
    ),
    cost = prop_number_decimal(NA_real_, allow_na = TRUE),
    duration = prop_number_decimal(NA_real_, allow_na = TRUE),
    role = new_property(
      class = class_character,
      getter = function(self) "assistant"
    )
  )
)
method(format, Turn) <- function(x, ...) {
  contents <- map_chr(x@contents, format, ...)
  paste0(contents, "\n", collapse = "")
}
method(contents_text, Turn) <- function(content) {
  paste0(unlist(lapply(content@contents, contents_text)), collapse = "")
}
method(contents_html, Turn) <- function(content) {
  paste0(unlist(lapply(content@contents, contents_html)), collapse = "\n")
}
method(contents_markdown, Turn) <- function(content) {
  paste0(unlist(lapply(content@contents, contents_markdown)), collapse = "\n\n")
}

method(print, Turn) <- function(x, ...) {
  cat(paste_c("<Turn: ", color_role(x@role), ">\n"))
  cat(format(x))
  invisible(x)
}

user_turn <- function(..., .call = caller_env(), .check_empty = TRUE) {
  as_user_turn(
    list2(...),
    call = .call,
    arg = "...",
    check_empty = .check_empty
  )
}

as_user_turn <- function(
  contents,
  check_empty = TRUE,
  call = caller_env(),
  arg = "..."
) {
  if (check_empty && length(contents) == 0) {
    cli::cli_abort("{.arg {arg}} must contain at least one input.", call = call)
  }
  if (is_named(contents)) {
    cli::cli_abort("{.arg {arg}} must be unnamed.", call = call)
  }
  if (S7_inherits(contents, Content)) {
    return(UserTurn(list(contents)))
  }

  contents <- lapply(contents, as_content, error_call = call, error_arg = arg)
  UserTurn(contents)
}

as_user_turns <- function(
  prompts,
  call = caller_env(),
  arg = caller_arg(prompts)
) {
  if (!is.list(prompts) && !is_prompt(prompts)) {
    stop_input_type(prompts, "a list or prompt", call = call, arg = arg)
  }
  turns <- map(seq_along(prompts), function(i) {
    this_arg <- paste0(arg, "[[", i, "]]")
    as_user_turn(prompts[[i]], call = call, arg = this_arg)
  })

  turns
}

is_system_turn <- function(x) {
  S7_inherits(x, SystemTurn)
}

is_user_turn <- function(x) {
  S7_inherits(x, UserTurn)
}

is_assistant_turn <- function(x) {
  S7_inherits(x, AssistantTurn)
}

check_turn <- function(x, call = caller_env(), arg = caller_arg(x)) {
  if (!S7_inherits(x, Turn)) {
    stop_input_type(x, "a <Turn>", call = call, arg = arg)
  }
}

normalize_turns <- function(
  turns = NULL,
  system_prompt = NULL,
  overwrite = FALSE,
  error_call = caller_env()
) {
  check_string(system_prompt, allow_null = TRUE)

  if (!is.null(turns)) {
    if (!is.list(turns) || is_named(turns)) {
      stop_input_type(
        turns,
        "an unnamed list",
        allow_null = TRUE,
        call = error_call
      )
    }
    correct_class <- map_lgl(turns, S7_inherits, Turn)
    if (!all(correct_class)) {
      cli::cli_abort("Every element of {.arg turns} must be a `turn`.")
    }
  } else {
    turns <- list()
  }

  if (!is.null(system_prompt)) {
    system_turn <- SystemTurn(system_prompt)

    # No turns; start with just the system prompt
    if (length(turns) == 0) {
      turns <- list(system_turn)
    } else if (!is_system_turn(turns[[1]])) {
      turns <- c(list(system_turn), turns)
    } else if (overwrite || identical(turns[[1]], system_turn)) {
      # Duplicate system prompt; don't need to do anything
    } else {
      cli::cli_abort(
        "`system_prompt` and `turns[[1]]` can't contain conflicting system prompts.",
        call = error_call
      )
    }
  }

  turns
}

turn_contents_preview <- function(turn) {
  is_text <- map_lgl(turn@contents, S7_inherits, ContentText)
  is_first_text <- is_text & cumsum(is_text) == 1

  contents <- map2_chr(turn@contents, is_first_text, \(x, is_first_text) {
    if (is_first_text) {
      paste0("Text[", str_trunc(x@text, 40), "]")
    } else {
      sub("^ellmer::Content", "", class(x)[[1]])
    }
  })
  paste(contents, collapse = ", ")
}
