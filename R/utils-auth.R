as_credentials <- function(
  fun_name,
  default,
  credentials = NULL,
  api_key = NULL,
  token = FALSE,
  env = caller_env(),
  user_env = caller_env(2)
) {
  if (is.null(credentials) && is.null(api_key)) {
    default
  } else if (!is.null(credentials) && is.null(api_key)) {
    check_credentials(credentials, error_call = env)
    credentials
  } else if (is.null(credentials) && !is.null(api_key)) {
    check_string(api_key, allow_null = TRUE, call = env)

    lifecycle::deprecate_warn(
      "0.4.0",
      paste0(fun_name, "(api_key)"),
      paste0(fun_name, "(credentials)"),
      env = env,
      user_env = user_env
    )
    if (token) {
      function() paste0("Bearer ", api_key)
    } else {
      function() api_key
    }
  } else {
    cli::cli_abort(
      "Must supply one of {.arg api_key} or {.arg credentials}.",
      call = env
    )
  }
}

check_credentials <- function(credentials, error_call = caller_env()) {
  check_function(credentials, allow_null = TRUE, call = error_call)
  if (length(formals(credentials)) != 0) {
    cli::cli_abort(
      "{.arg credentials} must not have arguments.",
      call = error_call
    )
  }

  creds <- credentials()
  if (!is_string(creds) && !(is_named(creds) && is.list(creds))) {
    stop_input_type(
      creds,
      c("a string", "a named list"),
      call = error_call,
      arg = "credentials()"
    )
  }

  invisible()
}

ellmer_req_credentials <- function(req, credentials, key_name = NULL) {
  if (is_string(credentials)) {
    if (is.null(key_name)) {
      cli::cli_abort(
        "A `credentials()` function that returns a string is not supported by this provider.",
        call = NULL
      )
    }
    credentials <- set_names(list(credentials), key_name)
  }
  req_headers_redacted(req, !!!credentials)
}
