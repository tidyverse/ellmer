is_testing <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")
}

is_snapshot <- function() {
  identical(Sys.getenv("TESTTHAT_IS_SNAPSHOT"), "true")
}

key_get <- function(name, error_call = caller_env()) {
  val <- Sys.getenv(name)
  if (!identical(val, "")) {
    val
  } else {
    if (is_testing()) {
      testthat::skip(sprintf("%s env var is not configured", name))
    } else {
      cli::cli_abort("Can't find env var {.code {name}}.", call = error_call)
    }
  }
}

key_exists <- function(name) {
  !identical(Sys.getenv(name), "")
}

defer <- function(expr, env = caller_env(), after = FALSE) {
  thunk <- as.call(list(function() expr))
  do.call(on.exit, list(thunk, TRUE, after), envir = env)
}

set_default <- function(value, default, arg = caller_arg(value)) {
  if (is.null(value)) {
    if (!is_testing() || is_snapshot()) {
      cli::cli_inform("Using {.field {arg}} = {.val {default}}.")
    }
    default
  } else {
    value
  }
}

last_request_json <- function() {
  print_json(last_request()$body$data)
}
last_response_json <- function() {
  print_json(resp_body_json(last_response()))
}
print_json <- function(x) {
  cat(pretty_json(x))
  cat("\n")
}
pretty_json <- function(x) {
  jsonlite::toJSON(x, pretty = TRUE, auto_unbox = TRUE)
}

prettify <- function(x) {
  tryCatch(
    jsonlite::prettify(x),
    error = function(cnd) x
  )
}

check_echo <- function(echo = NULL) {
  if (identical(echo, "text")) {
    lifecycle::deprecate_soft(
      when = "0.2.0",
      what = I('`echo = "text"`'),
      with = I('`echo = "output"`')
    )
    echo <- "output"
  }

  if (is.null(echo) || identical(echo, c("none", "output", "all"))) {
    if (env_is_user_facing(parent.frame(2)) && !is_testing()) {
      "output"
    } else {
      "none"
    }
  } else if (isTRUE(echo)) {
    "output"
  } else if (isFALSE(echo)) {
    "none"
  } else {
    arg_match(echo, c("none", "output", "all"))
  }
}

dots_named <- function(...) {
  is_named2(list2(...))
}

`paste<-` <- function(x, value) {
  paste0(x, value)
}

`append<-` <- function(x, value) {
  x[[length(x) + 1]] <- value
  x
}

#' Are credentials avaiable?
#'
#' Used for examples/testing.
#'
#' @keywords internal
#' @param provider Provider name.
#' @export
has_credentials <- function(provider) {
  switch(
    provider,
    cortex = cortex_credentials_exist(),
    openai = openai_key_exists(),
    claude = anthropic_key_exists(),
    cli::cli_abort("Unknown model {model}.")
  )
}

# In-memory cache for credentials. Analogous to httr2:::cache_mem().
credentials_cache <- function(key) {
  list(
    get = function() env_get(the$credentials_cache, key, default = NULL),
    set = function(creds) env_poke(the$credentials_cache, key, creds),
    clear = function() env_unbind(the$credentials_cache, key)
  )
}

has_connect_viewer_token <- function(...) {
  if (!is_installed("connectcreds")) {
    return(FALSE)
  }
  connectcreds::has_viewer_token(...)
}

modify_list <- function(x, y) {
  if (is.null(x)) {
    return(y)
  }
  if (is.null(y)) {
    return(x)
  }

  utils::modifyList(x, y)
}

is_whitespace <- function(x) {
  grepl("^(\\s|\n)*$", x)
}

paste_c <- function(...) {
  paste(c(...), collapse = "")
}

cli_escape <- function(x) {
  x <- gsub("{", "{{", x, fixed = TRUE)
  gsub("}", "}}", x, fixed = TRUE)
}

api_key_param <- function(key) {
  paste_c(
    "API key to use for authentication.\n",
    "\n",
    c(
      "You generally should not supply this directly, but instead set the ",
      c("`", key, "`"),
      " environment variable.\n"
    ),
    c(
      "The best place to set this is in `.Renviron`,
      which you can easily edit by calling `usethis::edit_r_environ()`."
    )
  )
}

param_model <- function(default, provider = NULL) {
  paste_c(
    c(
      "The model to use for the chat",
      if (!is.null(default)) c(" (defaults to \"", default, "\")"),
      ".\n"
    ),
    if (!is.null(default)) {
      c(
        "We regularly update the default, so we strongly recommend explicitly ",
        "specifying a model for anything other than casual use.\n"
      )
    },
    if (!is.null(provider)) {
      c("Use `models_", provider, "()` to see all options.\n")
    }
  )
}

unrowname <- function(df) {
  rownames(df) <- NULL
  df
}

color_role <- function(role) {
  switch(
    role,
    user = cli::col_blue(role),
    assistant = cli::col_green(role),
    system = cli::col_br_white(role),
    role
  )
}

counter <- function() {
  count <- 0
  function() {
    count <<- count + 1
    count
  }
}

match_prices <- function(provider, id) {
  prices <- prices[prices$provider == provider, ]
  idx <- match(id, prices$model)
  prices[idx, c("cached_input", "input", "output")]
}

base64_enc <- function(path, raw) {
  check_exclusive(path, raw)
  if (!missing(path)) {
    raw <- readBin(path, "raw", file.info(path)$size)
  }
  gsub("\n", "", openssl::base64_encode(raw), fixed = TRUE)
}
