#' @keywords internal
#' @importFrom R6 R6Class
"_PACKAGE"

the <- new_environment()
the$credentials_cache <- new_environment()

silence_r_cmd_check_note <- function() {
  later::later()
}

## usethis namespace: start
#' @import httr2
#' @import rlang
#' @import S7
#' @importFrom lifecycle deprecated
## usethis namespace: end
NULL
