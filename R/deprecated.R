#' Deprecated functions
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' ## Deprecated in v0.2.0
#'
#' * [chat_azure()] was renamed to [chat_azure_openai()].
#' * [chat_bedrock()] was renamed to [chat_aws_bedrock()].
#' * [chat_claude()] was renamed to [chat_anthropic()].
#' * [chat_gemini()] was renamed to [chat_google_gemini()].
#'
#' ## Deprecated in v0.1.1
#'
#' * [chat_cortex()] was renamed in v0.1.1 to [chat_cortex_analyst()] to
#'   distinguish it from the more general-purpose Snowflake Cortex chat
#'   function, [chat_snowflake()].
#'
#' @param ... Additional arguments passed from the deprecated function to its
#'   replacement.
#'
#' @keywords internal
#' @name deprecated
NULL


# Deprecated in v0.1.1 -----------------------------------------------------

#' @rdname deprecated
#' @export
chat_cortex <- function(...) {
  lifecycle::deprecate_warn("0.1.1", "chat_cortex()", "chat_cortex_analyst()")
  chat_cortex_analyst(...)
}

# Deprecated in v0.2.0 -----------------------------------------------------

#' @rdname deprecated
#' @export
chat_azure <- function(...) {
  lifecycle::deprecate_warn("0.2.0", "chat_azure()", "chat_azure_openai()")
  chat_azure_openai(...)
}

#' @rdname deprecated
#' @export
chat_bedrock <- function(...) {
  lifecycle::deprecate_warn("0.2.0", "chat_bedrock()", "chat_aws_bedrock()")
  chat_aws_bedrock(...)
}

#' @rdname deprecated
#' @export
chat_claude <- function(...) {
  lifecycle::deprecate_warn("0.2.0", "chat_claude()", "chat_anthropic()")
  chat_anthropic(...)
}

#' @rdname deprecated
#' @export
chat_gemini <- function(...) {
  lifecycle::deprecate_warn("0.2.0", "chat_gemini()", "chat_google_gemini()")
  chat_google_gemini(...)
}
