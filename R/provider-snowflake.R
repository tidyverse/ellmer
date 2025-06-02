#' @include provider-openai.R
#' @include content.R
NULL

#' Chat with a model hosted on Snowflake
#'
#' @description
#' The Snowflake provider allows you to interact with LLM models available
#' through the [Cortex LLM REST API](https://docs.snowflake.com/en/user-guide/snowflake-cortex/cortex-llm-rest-api).
#'
#' ## Authentication
#'
#' `chat_snowflake()` picks up the following ambient Snowflake credentials:
#'
#' - A static OAuth token defined via the `SNOWFLAKE_TOKEN` environment
#'   variable.
#' - Key-pair authentication credentials defined via the `SNOWFLAKE_USER` and
#'   `SNOWFLAKE_PRIVATE_KEY` (which can be a PEM-encoded private key or a path
#'   to one) environment variables.
#' - Posit Workbench-managed Snowflake credentials for the corresponding
#'   `account`.
#' - Viewer-based credentials on Posit Connect. Requires the \pkg{connectcreds}
#'   package.
#'
#' ## Known limitations
#' Note that Snowflake-hosted models do not support images.
#'
#' See [chat_cortex_analyst()] to chat with the Snowflake Cortex Analyst rather
#' than a general-purpose model.
#'
#' @inheritParams chat_openai
#' @inheritParams chat_cortex_analyst
#' @param model `r param_model("claude-3-7-sonnet")`
#' @inherit chat_openai return
#' @examplesIf has_credentials("cortex")
#' chat <- chat_snowflake()
#' chat$chat("Tell me a joke in the form of a SQL query.")
#' @export
chat_snowflake <- function(
  system_prompt = NULL,
  account = snowflake_account(),
  credentials = NULL,
  model = NULL,
  params = NULL,
  api_args = list(),
  echo = c("none", "output", "all")
) {
  check_string(account, allow_empty = FALSE)
  model <- set_default(model, "claude-3-7-sonnet")
  params <- params %||% params()
  echo <- check_echo(echo)

  if (is_list(credentials)) {
    static_credentials <- force(credentials)
    credentials <- function(account) static_credentials
  }
  check_function(credentials, allow_null = TRUE)
  credentials <- credentials %||% default_snowflake_credentials(account)

  provider <- ProviderSnowflakeCortex(
    name = "Snowflake/Cortex",
    base_url = snowflake_url(account),
    account = account,
    credentials = credentials,
    model = model,
    params = params,
    extra_args = api_args,
    # We need an empty api_key for S7 validation.
    api_key = ""
  )

  Chat$new(provider = provider, system_prompt = system_prompt, echo = echo)
}

ProviderSnowflakeCortex <- new_class(
  "ProviderSnowflakeCortex",
  parent = ProviderOpenAI,
  properties = list(
    account = prop_string(),
    credentials = class_function
  )
)

method(base_request, ProviderSnowflakeCortex) <- function(provider) {
  req <- request(provider@base_url)
  req <- ellmer_req_credentials(req, provider@credentials)
  req <- req_retry(req, max_tries = 2)
  req <- ellmer_req_timeout(req, stream)
  # Snowflake uses the User Agent header to identify "parter applications", so
  # identify requests as coming from "r_ellmer" (unless an explicit partner
  # application is set via the ambient SF_PARTNER environment variable).
  req <- ellmer_req_user_agent(req, Sys.getenv("SF_PARTNER"))

  # Snowflake-specific error response handling:
  req <- req_error(req, body = function(resp) resp_body_json(resp)$message)

  req
}

method(chat_path, ProviderSnowflakeCortex) <- function(provider) {
  "/api/v2/cortex/inference:complete"
}

# See: https://docs.snowflake.com/en/user-guide/snowflake-cortex/cortex-llm-rest-api#api-reference
method(chat_body, ProviderSnowflakeCortex) <- function(
  provider,
  stream = TRUE,
  turns = list(),
  tools = list(),
  type = NULL
) {
  messages <- as_json(provider, turns)
  tools <- as_json(provider, unname(tools))

  if (!is.null(type)) {
    # Note: Snowflake uses a slightly different format than OpenAI.
    response_format <- list(type = "json", schema = as_json(provider, type))
  } else {
    response_format <- NULL
  }

  params <- chat_params(provider, provider@params)
  compact(list2(
    messages = messages,
    model = provider@model,
    !!!params,
    stream = stream,
    tools = tools,
    response_format = response_format
  ))
}

method(as_json, list(ProviderSnowflakeCortex, TypeObject)) <- function(
  provider,
  x
) {
  # Unlike OpenAI, Snowflake does not support the "additionalProperties" field.
  names <- names2(x@properties)
  required <- map_lgl(x@properties, function(prop) prop@required)
  properties <- as_json(provider, x@properties)
  names(properties) <- names
  list(
    type = "object",
    description = x@description %||% "",
    properties = properties,
    required = as.list(names[required])
  )
}

# See: https://docs.snowflake.com/en/user-guide/snowflake-cortex/cortex-llm-rest-api#optional-json-arguments
method(chat_params, ProviderSnowflakeCortex) <- function(provider, params) {
  standardise_params(
    params,
    c(
      temperature = "temperature",
      top_p = "top_p",
      max_tokens = "max_tokens"
    )
  )
}

# Snowflake -> ellmer --------------------------------------------------------

method(stream_merge_chunks, ProviderSnowflakeCortex) <- function(
  provider,
  result,
  chunk
) {
  # We're aiming to make Snowflake's chunk format look the same as their non-
  # chunked format here so downstream processing logic can be uniform. We are
  # *not* trying to make it more sane.
  if (is.null(result)) {
    # Avoid multiple encodings for text content.
    if (chunk$choices[[1]]$delta$type == "text") {
      chunk$choices[[1]]$delta$type <- NULL
      chunk$choices[[1]]$delta$text <- NULL
    }
    # Non-streaming responses use "message" instead of "delta".
    chunk$choices[[1]]$message <- chunk$choices[[1]]$delta
    chunk$choices[[1]]$delta <- NULL
    return(chunk)
  }
  # Note: most fields are immutable between chunks and we can ignored updates.
  # We only care about changes to `choices[[1]]$delta` and `usage`.
  #
  # Note also: there is no index support in Snowflake's chunk format, so we're
  # always assuming we can operate on the first one, except in the special
  # case of tool calls.
  current <- result$choices[[1]]$message
  delta <- chunk$choices[[1]]$delta
  if (delta$type == "text") {
    paste(current$content_list[[1]]$text) <- delta$text
    current[["content"]] <- current$content_list[[1]]$text
  } else if (delta$type == "tool_use") {
    # When we get a tool call, we need to append a second entry to the content
    # list (again, since there is no index tracking).
    if (length(current$content_list) == 1) {
      current$content_list[[2]] <- list(
        type = "tool_use",
        tool_use = list(
          tool_use_id = delta$tool_use_id,
          name = delta$name,
          input = delta$input
        )
      )
    } else {
      # Otherwise we're appending to existing input.
      paste(current$content_list[[2]]$tool_use$input) <- delta$input
    }
  } else {
    cli::cli_abort(
      "Unsupported content type {.str {delta$type}}.",
      .internal = TRUE
    )
  }
  result$choices[[1]]$message <- current
  result$usage <- chunk$usage
  result
}

method(value_turn, ProviderSnowflakeCortex) <- function(
  provider,
  result,
  has_type = FALSE
) {
  raw_content <- result$choices[[1]]$message$content_list
  contents <- lapply(raw_content, function(content) {
    if (content$type == "text") {
      if (has_type) {
        ContentJson(jsonlite::parse_json(content$text))
      } else {
        ContentText(content$text)
      }
    } else if (content$type == "tool_use") {
      content <- content$tool_use
      if (is_string(content$input)) {
        content$input <- jsonlite::parse_json(content$input)
      }
      ContentToolRequest(
        content$tool_use_id,
        content$name,
        content$input %||% list()
      )
    } else {
      cli::cli_abort(
        "Unknown content type {.str {content$type}}.",
        .internal = TRUE
      )
    }
  })
  tokens <- tokens_log(
    provider,
    input = result$usage$prompt_tokens,
    output = result$usage$completion_tokens
  )
  assistant_turn(contents, json = result, tokens = tokens)
}

# ellmer -> Snowflake --------------------------------------------------------

method(as_json, list(ProviderSnowflakeCortex, Turn)) <- function(provider, x) {
  # Attempting to omit the `content` field and use `content_list` instead
  # yields:
  #
  # > messages[0].content cannot be empty string (was '')
  #
  # So we emulate what Snowflake do and put the text content in both.
  if (S7_inherits(x@contents[[1]], ContentText)) {
    content <- x@contents[[1]]@text
    if (nchar(content) == 0) {
      content <- "<empty>"
    }
  } else if (S7_inherits(x@contents[[1]], ContentJson)) {
    # Match the existing as_json() implementation.
    content <- "<structured data/>"
  } else if (S7_inherits(x@contents[[1]], ContentToolResult)) {
    # Completely undocumented, but: it seems like the model is expecting the
    # tool result in textual format here, too -- otherwise it gets confused,
    # like it can't see the output.
    content <- tool_string(x@contents[[1]])
  } else {
    cli::cli_abort("Unsupported content type: {.cls {class(x@contents[[1]])}}.")
  }
  list(
    role = x@role,
    content = content,
    content_list = as_json(provider, x@contents)
  )
}

# See: https://docs.snowflake.com/en/user-guide/snowflake-cortex/cortex-llm-rest-api#tools-configuration
method(as_json, list(ProviderSnowflakeCortex, ToolDef)) <- function(
  provider,
  x
) {
  list(
    tool_spec = compact(list(
      type = "generic",
      name = x@name,
      description = x@description,
      input_schema = as_json(provider, x@arguments)
    ))
  )
}

# See: https://docs.snowflake.com/en/user-guide/snowflake-cortex/cortex-llm-rest-api#tools-configuration
method(as_json, list(ProviderSnowflakeCortex, ContentToolRequest)) <- function(
  provider,
  x
) {
  input <- x@arguments
  if (length(input) == 0) {
    # Snowflake requires an empty object, rather than an empty array.
    input <- set_names(list())
  }
  list(
    type = "tool_use",
    tool_use = list(
      tool_use_id = x@id,
      name = x@name,
      input = input
    )
  )
}

# See: https://docs.snowflake.com/en/user-guide/snowflake-cortex/cortex-llm-rest-api#tool-results
method(as_json, list(ProviderSnowflakeCortex, ContentToolResult)) <- function(
  provider,
  x
) {
  list(
    type = "tool_results",
    tool_results = compact(list(
      tool_use_id = x@request@id,
      name = x@request@name,
      content = list(
        list(type = "text", text = tool_string(x))
      ),
      # TODO: Is this the correct format?
      status = if (tool_errored(x)) "error"
    ))
  )
}

# Utilities ------------------------------------------------------------------

snowflake_account <- function() {
  key_get("SNOWFLAKE_ACCOUNT")
}

snowflake_url <- function(account) {
  paste0("https://", account, ".snowflakecomputing.com")
}

default_snowflake_credentials <- function(account = snowflake_account()) {
  # Detect viewer-based credentials from Posit Connect.
  url <- snowflake_url(account)
  if (is_installed("connectcreds") && connectcreds::has_viewer_token(url)) {
    return(function() {
      token <- connectcreds::connect_viewer_token(url)
      list(
        Authorization = paste("Bearer", token$access_token),
        `X-Snowflake-Authorization-Token-Type` = "OAUTH"
      )
    })
  }

  token <- Sys.getenv("SNOWFLAKE_TOKEN")
  if (nchar(token) != 0) {
    return(function() {
      list(
        Authorization = paste("Bearer", token),
        # See: https://docs.snowflake.com/en/developer-guide/snowflake-rest-api/authentication#using-oauth
        `X-Snowflake-Authorization-Token-Type` = "OAUTH"
      )
    })
  }

  # Support for Snowflake key-pair authentication.
  # See: https://docs.snowflake.com/en/developer-guide/snowflake-rest-api/authentication#generate-a-jwt-token
  user <- Sys.getenv("SNOWFLAKE_USER")
  private_key <- Sys.getenv("SNOWFLAKE_PRIVATE_KEY")
  if (nchar(user) != 0 && nchar(private_key) != 0) {
    check_installed("jose", "for key-pair authentication")
    key <- openssl::read_key(private_key)
    return(function() {
      token <- snowflake_keypair_token(account, user, key)
      list(
        Authorization = paste("Bearer", token),
        `X-Snowflake-Authorization-Token-Type` = "KEYPAIR_JWT"
      )
    })
  }

  # Check for Workbench-managed credentials.
  sf_home <- Sys.getenv("SNOWFLAKE_HOME")
  if (grepl("posit-workbench", sf_home, fixed = TRUE)) {
    token <- workbench_snowflake_token(account, sf_home)
    if (!is.null(token)) {
      return(function() {
        # Ensure we get an up-to-date token.
        token <- workbench_snowflake_token(account, sf_home)
        list(
          Authorization = paste("Bearer", token),
          `X-Snowflake-Authorization-Token-Type` = "OAUTH"
        )
      })
    }
  }

  if (is_testing()) {
    testthat::skip("no Snowflake credentials available")
  }

  cli::cli_abort("No Snowflake credentials are available.")
}

snowflake_keypair_token <- function(
  account,
  user,
  key,
  cache = snowflake_keypair_cache(account, key),
  lifetime = 600L,
  reauth = FALSE
) {
  # Producing a signed JWT is a fairly expensive operation (in the order of
  # ~10ms), but adding a cache speeds this up approximately 500x.
  creds <- cache$get()
  if (reauth || is.null(creds) || creds$expiry < Sys.time()) {
    cache$clear()
    expiry <- Sys.time() + lifetime
    # We can't use openssl::fingerprint() here because it uses a different
    # algorithm.
    fp <- openssl::base64_encode(
      openssl::sha256(openssl::write_der(key$pubkey))
    )
    sub <- toupper(paste0(account, ".", user))
    iss <- paste0(sub, ".SHA256:", fp)
    # Note: Snowflake employs a malformed issuer claim, so we have to inject it
    # manually after jose's validation phase.
    claim <- jwt_claim("dummy", sub, exp = as.integer(expiry))
    claim$iss <- iss
    creds <- list(expiry = expiry, token = jwt_encode_sig(claim, key))
    cache$set(creds)
  }
  creds$token
}

snowflake_keypair_cache <- function(account, key) {
  credentials_cache(key = hash(c("sf", account, openssl::fingerprint(key))))
}
