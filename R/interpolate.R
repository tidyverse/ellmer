#' Helpers for interpolating data into prompts
#'
#' @description
#' These functions are lightweight wrappers around
#' [glue](https://glue.tidyverse.org/) that make it easier to interpolate
#' dynamic data into a static prompt:
#'
#' * `interpolate()` works with a string.
#' * `interpolate_file()` works with a file.
#' * `interpolate_package()` works with a file in the `insts/prompt`
#'   directory of a package.
#'
#' Compared to glue, dynamic values should be wrapped in `{{ }}`, making it
#' easier to include R code and JSON in your prompt.
#'
#' @param prompt A prompt string. You should not generally expose this
#'   to the end user, since glue interpolation makes it easy to run arbitrary
#'   code.
#' @param ... Define additional temporary variables for substitution.
#' @param .envir Environment to evaluate `...` expressions in. Used when
#'   wrapping in another function. See `vignette("wrappers", package = "glue")`
#'   for more details.
#' @return A \{glue\} string.
#' @export
#' @examples
#' joke <- "You're a cool dude who loves to make jokes. Tell me a joke about {{topic}}."
#'
#' # You can supply valuese directly:
#' interpolate(joke, topic = "bananas")
#'
#' # Or allow interpolate to find them in the current environment:
#' topic <- "applies"
#' interpolate(joke)
#'
#'
interpolate <- function(prompt, ..., .envir = parent.frame()) {
  check_string(prompt)

  dots <- list2(...)
  if (length(dots) > 0 && !is_named(dots)) {
    cli::cli_abort("All elements of `...` must be named")
  }

  envir <- list2env(dots, parent = .envir)
  out <- glue::glue(prompt, .open = "{{", .close = "}}", .envir = envir)
  ellmer_prompt(out)
}

#' @param path A path to a prompt file (often a `.md`).
#' @rdname interpolate
#' @export
interpolate_file <- function(path, ..., .envir = parent.frame()) {
  string <- read_file(path)
  interpolate(string, ..., .envir = .envir)
}

#' @param package Package name.
#' @rdname interpolate
#' @export
interpolate_package <- function(
  package,
  path,
  ...,
  .envir = parent.frame()
) {
  path <- system.file("prompts", path, package = package)
  interpolate_file(path, ..., .envir = .envir)
}

read_file <- function(path) {
  file_contents <- readChar(path, file.size(path))
}

# Prompt class -----------------------------------------------------------------

ellmer_prompt <- function(x) {
  structure(x, class = c("ellmer_prompt", "character"))
}

is_prompt <- function(x) {
  inherits(x, "ellmer_prompt")
}

#' @export
print.ellmer_prompt <- function(
  x,
  ...,
  max_items = 20,
  max_lines = max_items * 10
) {
  n <- length(x)
  n_extra <- length(x) - max_items
  if (n_extra > 0) {
    x <- x[seq_len(max_items)]
  }

  if (length(x) == 0) {
    cli::cli_inform(c(x = "Zero-length prompt.\n"))
    return(invisible(x))
  }

  bar <- if (cli::is_utf8_output()) "\u2502" else "|"

  id <- format(paste0("[", seq_along(x), "] "), justify = "right")
  indent <- paste0(cli::col_grey(id, bar), " ")
  exdent <- paste0(strrep(" ", nchar(id[[1]])), cli::col_grey(bar), " ")

  x[is.na(x)] <- cli::col_red("NA")
  x <- paste0(indent, x)
  x <- gsub("\n", paste0("\n", exdent), x)

  lines <- strsplit(x, "\n")
  ids <- rep(seq_along(x), length(lines))
  lines <- unlist(lines)

  if (length(lines) > max_lines) {
    lines <- lines[seq_len(max_lines)]
    lines <- c(lines, paste0(exdent, "..."))
    n_extra <- n - ids[max_lines - 1]
  }

  cat(lines, sep = "\n")
  if (n_extra > 0) {
    cat("... and ", n_extra, " more.\n", sep = "")
  }

  invisible(x)
}

#' @export
`[.ellmer_prompt` <- function(x, i, ...) {
  ellmer_prompt(NextMethod())
}

# Helpers ----------------------------------------------------------------------

# for mocking
system.file <- NULL
