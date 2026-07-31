request_reauthenticate <- function(provider, req) {
  if (!S7_inherits(provider, ProviderOpenAI) || provider@auth != "codex") {
    return(NULL)
  }

  codex_refresh()
  ellmer_req_credentials(req, provider@credentials(), "Authorization")
}

codex_credentials <- function() {
  \(req) req_headers_redacted(req, !!!codex_auth())
}

codex_auth <- function() {
  home <- Sys.getenv("CODEX_HOME", "")
  if (home == "") {
    home <- path.expand("~/.codex")
  }
  path <- file.path(home, "auth.json")
  if (!file.exists(path)) {
    cli::cli_abort(c(
      "Can't find file-backed Codex authentication at {.path {path}}.",
      "i" = "Run {.code codex login} with {.code cli_auth_credentials_store = \"file\"}."
    ))
  }

  auth <- tryCatch(
    jsonlite::read_json(path, simplifyVector = FALSE),
    error = function(cnd) NULL
  )
  if (is.null(auth)) {
    cli::cli_abort("Can't read Codex authentication at {.path {path}}.")
  }
  tokens <- auth$tokens
  if (
    !identical(auth$auth_mode, "chatgpt") ||
      !is_string(tokens$access_token) ||
      !is_string(tokens$account_id) ||
      !is_string(tokens$id_token)
  ) {
    cli::cli_abort(
      "{.path {path}} does not contain file-backed Codex ChatGPT authentication."
    )
  }

  compact(list(
    Authorization = paste("Bearer", tokens$access_token),
    `ChatGPT-Account-ID` = tokens$account_id,
    `X-OpenAI-Fedramp` = if (codex_is_fedramp(tokens$id_token)) "true"
  ))
}

codex_is_fedramp <- function(token) {
  parts <- strsplit(token, ".", fixed = TRUE)[[1]]
  if (length(parts) != 3) {
    cli::cli_abort("Invalid ID token in Codex authentication.")
  }

  payload <- chartr("-_", "+/", parts[[2]])
  payload <- paste0(payload, strrep("=", (4 - nchar(payload) %% 4) %% 4))
  claims <- tryCatch(
    jsonlite::fromJSON(
      rawToChar(jsonlite::base64_dec(payload)),
      simplifyVector = FALSE
    ),
    error = function(cnd) NULL
  )
  if (is.null(claims)) {
    cli::cli_abort("Invalid ID token in Codex authentication.")
  }
  isTRUE(
    claims[["https://api.openai.com/auth"]]$chatgpt_account_is_fedramp
  )
}

codex_refresh <- function() {
  codex <- unname(Sys.which("codex"))
  if (codex == "") {
    cli::cli_abort(
      "Can't refresh authentication because Codex CLI is not installed."
    )
  }

  empty_object <- structure(list(), names = character())
  process <- processx::process$new(
    codex,
    c("app-server", "-c", "cli_auth_credentials_store=file"),
    stdin = "|",
    stdout = "|",
    stderr = "|"
  )
  on.exit(if (process$is_alive()) process$kill(), add = TRUE)

  codex_send(
    process,
    list(
      method = "initialize",
      id = 0L,
      params = list(
        clientInfo = list(
          name = "ellmer",
          title = "ellmer",
          version = as.character(utils::packageVersion("ellmer"))
        )
      )
    )
  )
  initialized <- codex_read_response(process, 0L)
  if (!is.null(initialized$error)) {
    cli::cli_abort("Codex CLI failed to initialize authentication refresh.")
  }

  codex_send(process, list(method = "initialized", params = empty_object))
  codex_send(
    process,
    list(
      method = "account/read",
      id = 1L,
      params = list(refreshToken = TRUE)
    )
  )
  response <- codex_read_response(process, 1L)
  close(process$get_input_connection())

  if (!is.null(response$error)) {
    cli::cli_abort("Codex CLI failed to refresh ChatGPT authentication.")
  }
  invisible()
}

codex_send <- function(process, message) {
  json <- jsonlite::toJSON(message, auto_unbox = TRUE)
  process$write_input(paste0(json, "\n"))
}

codex_read_response <- function(process, id, timeout = 30) {
  for (i in seq_len(timeout)) {
    poll <- process$poll_io(1000)
    if (poll[["error"]] %in% c("ready", "closed")) {
      process$read_error()
    }
    if (poll[["output"]] %in% c("ready", "closed")) {
      output <- process$read_output_lines()
      responses <- lapply(output, jsonlite::fromJSON, simplifyVector = FALSE)
      response <- Filter(\(x) isTRUE(x$id == id), responses)
      if (length(response) == 1) {
        return(response[[1]])
      }
    }

    if (!process$is_alive()) {
      cli::cli_abort("Codex CLI exited before responding.")
    }
  }
  cli::cli_abort("Timed out waiting for Codex CLI.")
}
