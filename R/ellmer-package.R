#' @keywords internal
#' @importFrom R6 R6Class
"_PACKAGE"

the <- new_environment()
the$credentials_cache <- new_environment()
the$tool_context_stack <- list()

silence_r_cmd_check_note <- function() {
  later::later()
}

release_bullets <- function() {
  c(
    "Review the default models and update where appropriate",
    "Delete VCR cassettes, re-record with `vcr_rebuild()`, and review diffs",
    "Run revdep checks and notify maintainers of any failures"
  )
}

## usethis namespace: start
#' @import httr2
#' @import rlang
#' @rawNamespace import(S7, except = ":=")
#' @importFrom lifecycle deprecated
## usethis namespace: end
NULL
