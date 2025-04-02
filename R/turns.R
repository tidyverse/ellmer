#' @include utils-S7.R
NULL

#' A user or assistant turn
#'
#' @description
#' Every conversation with a chatbot consists of pairs of user and assistant
#' turns, corresponding to an HTTP request and response. These turns are
#' represented by the `Turn` object, which contains a list of [Content]s representing
#' the individual messages within the turn. These might be text, images, tool
#' requests (assistant only), or tool responses (user only).
#'
#' Note that a call to `$chat()` and related functions may result in multiple
#' user-assistant turn cycles. For example, if you have registered tools,
#' ellmer will automatically handle the tool calling loop, which may result in
#' any number of additional cycles. Learn more about tool calling in
#' `vignette("tool-calling")`.
#'
#' @param role Either "user", "assistant", or "system".
#' @param contents A list of [Content] objects.
#' @param json The serialized JSON corresponding to the underlying data of
#'   the turns. Currently only provided for assistant.
#'
#'   This is useful if there's information returned by the provider that ellmer
#'   doesn't otherwise expose.
#' @param tokens A numeric vector of length 2 representing the number of
#'   input and output tokens (respectively) used in this turn. Currently
#'   only recorded for assistant turns.
#' @param completed A POSIXct timestamp indicating when the turn completed.
#' @export
#' @return An S7 `Turn` object
#' @examples
#' Turn(role = "user", contents = list(ContentText("Hello, world!")))
Turn <- new_class(
  "Turn",
  properties = list(
    role = prop_string(),
    contents = prop_list_of(Content),
    json = class_list,
    tokens = new_property(
      class_numeric,
      default = c(NA_real_, NA_real_),
      validator = function(value) {
        if (length(value) != 2) {
          "must be length two"
        }
      }
    ),
    text = new_property(
      class = class_character,
      getter = function(self) contents_text(self)
    ),
    completed = new_property(class = class_POSIXct | NULL)
  ),
  constructor = function(
    role,
    contents = list(),
    json = list(),
    tokens = c(0, 0),
    completed = Sys.time()
  ) {
    if (is.character(contents)) {
      contents <- list(ContentText(paste0(contents, collapse = "\n")))
    }
    new_object(
      S7_object(),
      role = role,
      contents = contents,
      json = json,
      tokens = tokens,
      completed = completed
    )
  }
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

user_turn <- function(..., .call = caller_env()) {
  as_user_turn(list2(...), call = .call, arg = "...")
}

as_user_turn <- function(contents, call = caller_env(), arg = "...") {
  if (length(contents) == 0) {
    cli::cli_abort("{.arg {arg}} must contain at least one input.", call = call)
  }
  if (is_named(contents)) {
    cli::cli_abort("{.arg {arg}} must be unnamed.", call = call)
  }

  contents <- lapply(contents, as_content, error_call = call, error_arg = arg)
  Turn("user", contents)
}

as_user_turns <- function(
  prompts,
  call = caller_env(),
  arg = caller_arg(prompts)
) {
  if (!is.list(prompts)) {
    stop_input_type(prompts, "a list", call = call, arg = arg)
  }
  turns <- map(seq_along(prompts), function(i) {
    this_arg <- paste0(arg, "[[", i, "]]")
    as_user_turn(prompts[[i]], call = call, arg = this_arg)
  })

  turns
}

is_system_prompt <- function(x) {
  x@role == "system"
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
  check_character(system_prompt, allow_null = TRUE, call = error_call)
  if (length(system_prompt) > 1) {
    system_prompt <- paste(system_prompt, collapse = "\n\n")
  }

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
    system_turn <- Turn("system", system_prompt, completed = NULL)

    # No turns; start with just the system prompt
    if (length(turns) == 0) {
      turns <- list(system_turn)
    } else if (turns[[1]]@role != "system") {
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
