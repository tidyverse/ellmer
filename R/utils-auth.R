as_credentials <- function(
  fun_name,
  default,
  credentials = NULL,
  api_key = NULL,
  env = caller_env(),
  user_env = caller_env(2)
) {
  if (is.null(credentials) && is.null(api_key)) {
    default
  } else if (!is.null(credentials) && is.null(api_key)) {
    credentials
  } else if (is.null(credentials) && !is.null(api_key)) {
    lifecycle::deprecate_warn(
      "0.4.0",
      paste0(fun_name, "(api_key)"),
      paste0(fun_name, "(credentials)"),
      env = env,
      user_env = user_env
    )
    function() api_key
  } else {
    cli::cli_abort(
      "Must supply one of {.arg api_key} or {.arg credentials}.",
      call = env
    )
  }
}
