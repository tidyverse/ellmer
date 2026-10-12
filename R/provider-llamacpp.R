#' Chat with a local llama.cpp server
#'
#' @description
#' To use `chat_llamacpp()` first download and install
#' [llama.cpp](https://github.com/ggerganov/llama.cpp). Then start the server
#' with a GGUF model file:
#'
#' ```bash
#' ./llama-server -m /path/to/model.gguf
#' ```
#'
#' llama.cpp can load GGUF files directly, including models from your local
#' Ollama installation (typically in `~/.ollama/models/blobs/`), allowing you
#' to reuse already-downloaded models without duplicating storage.
#'
#' Built on top of [chat_openai_compatible()].
#'
#' ## Known limitations
#'
#' * Tool calling support depends on the specific model's capabilities
#' * Only one model can be loaded at a time (unless using multiple server instances)
#'
#' @inheritParams chat_openai
#' @param model The model name. If `NULL` (default), automatically uses
#'   whichever model is currently loaded by the llama.cpp server.
#' @param base_url The base URL for the llama.cpp server. Defaults to
#'   `http://localhost:8080` or the `LLAMACPP_BASE_URL` environment variable.
#' @param api_key `r lifecycle::badge("deprecated")` Use `credentials` instead.
#' @param credentials llama.cpp doesn't require credentials for local usage
#'   and in most cases you do not need to provide `credentials`.
#'
#'   However, if you're accessing a llama.cpp instance hosted behind a reverse
#'   proxy or secured endpoint that enforces bearer‐token authentication, you
#'   can set the `LLAMACPP_API_KEY` environment variable or provide a callback
#'   function to `credentials`.
#' @param params Common model parameters, usually created by [params()].
#' @inherit chat_openai return
#' @family chatbots
#' @export
#' @examples
#' \dontrun{
#' # Start llama.cpp server first:
#' # ./llama-server -m /path/to/model.gguf
#'
#' # Auto-detect loaded model
#' chat <- chat_llamacpp()
#' chat$chat("Tell me three jokes about statisticians")
#'
#' # Or specify model name explicitly
#' chat <- chat_llamacpp(model = "llama-3.2-3b")
#'
#' # Use an Ollama model with llama.cpp
#' # (find local GGUF files with list_llamacpp_models())
#' # ./llama-server -m ~/.ollama/models/blobs/sha256-abc123...
#' }
chat_llamacpp <- function(
  system_prompt = NULL,
  base_url = Sys.getenv("LLAMACPP_BASE_URL", "http://localhost:8080"),
  model = NULL,
  params = NULL,
  api_args = list(),
  echo = NULL,
  api_key = NULL,
  credentials = NULL,
  api_headers = character()
) {
  credentials <- llamacpp_credentials(credentials, api_key)

  if (!has_llamacpp(base_url, credentials)) {
    cli::cli_abort(c(
      "Can't find running llama.cpp server at {.val {base_url}}.",
      i = "Start the server with: {.code ./llama-server -m /path/to/model.gguf}",
      i = "Or set {.envvar LLAMACPP_BASE_URL} to the correct URL."
    ))
  }

  models <- models_llamacpp(base_url, credentials)

  if (is.null(model)) {
    if (nrow(models) == 0) {
      cli::cli_abort(c(
        "No models are currently loaded in the llama.cpp server.",
        i = "Start the server with a model: {.code ./llama-server -m /path/to/model.gguf}"
      ))
    }
    # Use the first (and typically only) loaded model
    model <- models$id[1]
    cli::cli_inform("Using loaded model: {.val {model}}")
  } else if (!model %in% models$id) {
    cli::cli_warn(c(
      "Model {.val {model}} not found in server's loaded models.",
      i = "Server has: {.val {models$id}}",
      i = "Proceeding anyway - the server may still accept this model name."
    ))
  }

  echo <- check_echo(echo)

  provider <- ProviderLlamaCpp(
    name = "LlamaCpp",
    base_url = file.path(base_url, "v1"),
    model = model,
    params = params %||% params(),
    extra_args = api_args,
    credentials = credentials,
    extra_headers = api_headers
  )

  Chat$new(provider = provider, system_prompt = system_prompt, echo = echo)
}

ProviderLlamaCpp <- new_class(
  "ProviderLlamaCpp",
  parent = ProviderOpenAICompatible,
  properties = list(
    model = prop_string()
  )
)

llamacpp_credentials <- function(credentials = NULL, api_key = NULL) {
  as_credentials(
    "chat_llamacpp",
    function() Sys.getenv("LLAMACPP_API_KEY", ""),
    credentials = credentials,
    api_key = api_key
  )
}

method(chat_params, ProviderLlamaCpp) <- function(provider, params) {
  # llama.cpp server supports OpenAI-compatible parameters plus some extras
  # https://github.com/ggerganov/llama.cpp/blob/master/examples/server/README.md
  standardise_params(
    params,
    c(
      frequency_penalty = "frequency_penalty",
      presence_penalty = "presence_penalty",
      seed = "seed",
      stop = "stop_sequences",
      temperature = "temperature",
      top_p = "top_p",
      top_k = "top_k",
      max_tokens = "max_tokens"
      # llama.cpp specific parameters
      #repeat_penalty = "repeat_penalty",
      #mirostat = "mirostat",
      #mirostat_tau = "mirostat_tau",
      #mirostat_eta = "mirostat_eta",
      #n_probs = "n_probs",
      #min_p = "min_p"
    )
  )
}

chat_llamacpp_test <- function(..., model = NULL, echo = "none") {
  skip_if_no_llamacpp()
  chat_llamacpp(..., model = model, echo = echo)
}

skip_if_no_llamacpp <- function() {
  if (!has_llamacpp()) {
    testthat::skip("llama.cpp server not found")
  }
}

#' List models loaded in llama.cpp server
#'
#' @description
#' Queries the llama.cpp server to see which models are currently loaded.
#' Unlike Ollama, llama.cpp typically loads a single model at server startup.
#'
#' @export
#' @rdname chat_llamacpp
#' @param base_url The base URL for the llama.cpp server.
#' @param credentials Optional credentials for authenticated access.
#' @return A data frame with columns `id`, `created_at`, and `owned_by`.
models_llamacpp <- function(
  base_url = Sys.getenv("LLAMACPP_BASE_URL", "http://localhost:8080"),
  credentials = NULL
) {
  credentials <- as_credentials(
    "models_llamacpp",
    function() Sys.getenv("LLAMACPP_API_KEY", ""),
    credentials = credentials
  )

  # llama.cpp exposes an OpenAI-compatible /v1/models endpoint
  req <- request(base_url)
  req <- ellmer_req_credentials(req, credentials(), "Authorization")
  req <- req_url_path_append(req, "v1/models")

  resp <- tryCatch(
    req_perform(req),
    error = function(e) {
      cli::cli_abort(c(
        "Failed to connect to llama.cpp server at {.val {base_url}}.",
        i = "Make sure the server is running.",
        x = conditionMessage(e)
      ))
    }
  )

  json <- resp_body_json(resp)

  if (length(json$data) == 0) {
    return(data.frame(
      id = character(0),
      created_at = as.POSIXct(character(0)),
      owned_by = character(0),
      stringsAsFactors = FALSE
    ))
  }

  models_data <- json$data

  data.frame(
    id = map_chr(models_data, function(x) x$id %||% "unknown"),
    created_at = as.POSIXct(
      map_dbl(models_data, function(x) x$created %||% 0),
      origin = "1970-01-01"
    ),
    owned_by = map_chr(models_data, function(x) x$owned_by %||% "llamacpp"),
    stringsAsFactors = FALSE
  )
}

has_llamacpp <- function(
  base_url = Sys.getenv("LLAMACPP_BASE_URL", "http://localhost:8080"),
  credentials = llamacpp_credentials()
) {
  check_credentials(credentials)

  tryCatch(
    {
      req <- request(base_url)
      req <- ellmer_req_credentials(req, credentials(), "Authorization")
      # Use /v1/models as health check
      req <- req_url_path_append(req, "v1/models")
      req_perform(req)
      TRUE
    },
    error = function(e) FALSE
  )
}


#' List GGUF models available on this machine
#'
#' @description
#' Scans the machine for GGUF model files that can be loaded by llama.cpp
#' (`./llama-server -m /path/to/model.gguf`). Files are identified by their
#' GGUF magic bytes, so files without a `.gguf` extension are found too --
#' including Ollama's blob store, letting you reuse already-downloaded Ollama
#' models without duplicating storage.
#'
#' By default the scan covers:
#' * Ollama's blob store (`~/.ollama/models/blobs/`, or
#'   `%USERPROFILE%\\.ollama\\models\\blobs\\` on Windows, or
#'   `$OLLAMA_MODELS/blobs` when that variable is set),
#' * the llama.cpp download cache (`~/.cache/llama.cpp/`),
#' * `~/models/`,
#' * any extra directories passed via `dirs`.
#'
#' This is the llama.cpp analogue of [models_ollama()]: it tells you what you
#' *could* load, while `models_llamacpp()` tells you what the server *has*
#' loaded.
#'
#' @param dirs Additional directories to scan (in addition to the defaults).
#'   Non-existent directories are silently skipped.
#' @param recursive Scan directories recursively. Defaults to `TRUE`.
#' @returns A data frame with columns `name` (file name), `path` (full path,
#'   ready to pass to `./llama-server -m`), and `size` (file size in bytes).
#'   An empty data frame when nothing is found.
#' @export
#' @examples
#' \dontrun{
#' list_llamacpp_models()
#'
#' # start the server with one of the results
#' # ./llama-server -m /path/to/model.gguf
#' }
list_llamacpp_models <- function(dirs = NULL, recursive = TRUE) {
  scan_dirs <- llamacpp_default_dirs(dirs)
  if (length(scan_dirs) == 0L) {
    return(llamacpp_empty_models())
  }

  files <- unlist(
    lapply(scan_dirs, function(d) {
      list.files(d, full.names = TRUE, recursive = recursive, all.files = FALSE)
    }),
    use.names = FALSE
  )
  files <- files[!dir.exists(files)]
  is_gguf <- vapply(files, llamacpp_is_gguf, logical(1), USE.NAMES = FALSE)
  files <- files[is_gguf]

  if (length(files) == 0L) {
    return(llamacpp_empty_models())
  }

  info <- file.info(files)
  df <- data.frame(
    name = basename(files),
    path = files,
    size = info$size,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  df[order(df$name), , drop = FALSE]
}

# Default directories to scan for GGUF files
llamacpp_default_dirs <- function(dirs = NULL) {
  home <- Sys.getenv("HOME", "~")
  if (identical(.Platform$OS.type, "windows")) {
    home <- Sys.getenv("USERPROFILE", home)
  }

  ollama_models <- Sys.getenv("OLLAMA_MODELS", "")
  if (!nzchar(ollama_models)) {
    ollama_models <- file.path(home, ".ollama", "models")
  }

  candidates <- c(
    file.path(ollama_models, "blobs"),
    file.path(home, ".cache", "llama.cpp"),
    file.path(home, "models"),
    dirs
  )
  unique(candidates[dir.exists(candidates)])
}

# Check GGUF magic bytes (first 4 bytes are "GGUF")
llamacpp_is_gguf <- function(path) {
  con <- tryCatch(
    suppressWarnings(file(path, "rb")),
    error = function(e) NULL
  )
  if (is.null(con)) {
    return(FALSE)
  }
  on.exit(tryCatch(close(con), error = function(e) NULL), add = TRUE)
  magic <- tryCatch(readBin(con, "raw", n = 4L), error = function(e) raw(0))
  length(magic) == 4L && all(magic == charToRaw("GGUF"))
}

llamacpp_empty_models <- function() {
  data.frame(
    name = character(0),
    path = character(0),
    size = numeric(0),
    stringsAsFactors = FALSE
  )
}
