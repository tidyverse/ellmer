#' Chat with a model hosted on Databricks
#'
#' @description
#' Databricks provides out-of-the-box access to a number of [foundation
#' models](https://docs.databricks.com/en/machine-learning/model-serving/score-foundation-models.html)
#' and can also serve as a gateway for external models hosted by a third party.
#'
#' ## Authentication
#'
#' `chat_databricks()` picks up on ambient Databricks credentials for a subset
#' of the [Databricks client unified
#' authentication](https://docs.databricks.com/en/dev-tools/auth/unified-auth.html)
#' model. Specifically, it supports:
#'
#' - Personal access tokens
#' - Service principals via OAuth (OAuth M2M)
#' - User account via OAuth (OAuth U2M)
#' - Authentication via the Databricks CLI
#' - Posit Workbench-managed credentials
#' - Viewer-based credentials on Posit Connect. Requires the \pkg{connectcreds}
#'   package.
#'
#' ## Known limitations
#'
#' Databricks models do not support images, but they do support structured
#' outputs. Tool calling support is also very limited at present and is
#' currently not supported by ellmer.
#'
#' @family chatbots
#' @param workspace The URL of a Databricks workspace, e.g.
#'   `"https://example.cloud.databricks.com"`. Will use the value of the
#'   environment variable `DATABRICKS_HOST`, if set.
#' @param model `r param_model("databricks-dbrx-instruct")`
#'
#'   Available foundational models include:
#'
#'   - `databricks-dbrx-instruct` (the default)
#'   - `databricks-mixtral-8x7b-instruct`
#'   - `databricks-meta-llama-3-1-70b-instruct`
#'   - `databricks-meta-llama-3-1-405b-instruct`
#' @param token An authentication token for the Databricks workspace, or
#'   `NULL` to use ambient credentials.
#' @inheritParams chat_openai
#' @inherit chat_openai return
#' @export
#' @examples
#' \dontrun{
#' chat <- chat_databricks()
#' chat$chat("Tell me three jokes about statisticians")
#' }
chat_databricks <- function(
  workspace = databricks_workspace(),
  system_prompt = NULL,
  model = NULL,
  token = NULL,
  api_args = list(),
  echo = c("none", "output", "all")
) {
  check_string(workspace, allow_empty = FALSE)
  check_string(token, allow_empty = FALSE, allow_null = TRUE)
  model <- set_default(model, "databricks-dbrx-instruct")
  echo <- check_echo(echo)
  if (!is.null(token)) {
    credentials <- function() list(Authorization = paste("Bearer", token))
  } else {
    credentials <- default_databricks_credentials(workspace)
  }
  provider <- ProviderDatabricks(
    name = "Databricks",
    base_url = workspace,
    model = model,
    extra_args = api_args,
    credentials = credentials,
    # Databricks APIs use bearer tokens, not API keys, but we need to pass an
    # empty string here anyway to make S7::validate() happy.
    api_key = ""
  )
  Chat$new(provider = provider, system_prompt = system_prompt, echo = echo)
}

ProviderDatabricks <- new_class(
  "ProviderDatabricks",
  parent = ProviderOpenAI,
  properties = list(credentials = class_function)
)

method(base_request, ProviderDatabricks) <- function(provider) {
  req <- request(provider@base_url)
  req <- ellmer_req_credentials(req, provider@credentials)
  req <- req_retry(req, max_tries = 2)
  req <- ellmer_req_timeout(req, stream)
  req <- ellmer_req_user_agent(req, databricks_user_agent())
  req <- base_request_error(provider, req)
  req
}

method(chat_body, ProviderDatabricks) <- function(
  provider,
  stream = TRUE,
  turns = list(),
  tools = list(),
  type = NULL
) {
  body <- chat_body(
    super(provider, ProviderOpenAI),
    stream = stream,
    turns = turns,
    tools = tools,
    type = type
  )

  # Databricks doensn't support stream options
  body$stream_options <- NULL

  body
}

method(chat_path, ProviderDatabricks) <- function(provider) {
  # Note: this API endpoint is undocumented and seems to exist primarily for
  # compatibility with the OpenAI Python SDK. The documented endpoint is
  # `/serving-endpoints/<model>/invocations`.
  "/serving-endpoints/chat/completions"
}

method(base_request_error, ProviderDatabricks) <- function(provider, req) {
  req_error(req, body = function(resp) {
    if (resp_content_type(resp) == "application/json") {
      # Databrick's "OpenAI-compatible" API has a slightly incompatible error
      # response format, which we account for here.
      resp_body_json(resp)$message
    }
  })
}

method(as_json, list(ProviderDatabricks, Turn)) <- function(provider, x) {
  if (x@role == "system") {
    list(list(role = "system", content = x@contents[[1]]@text))
  } else if (x@role == "user") {
    # Each tool result needs to go in its own message with role "tool".
    is_tool <- map_lgl(x@contents, S7_inherits, ContentToolResult)
    if (any(is_tool)) {
      return(lapply(x@contents[is_tool], function(tool) {
        list(
          role = "tool",
          content = tool_string(tool),
          tool_call_id = tool@request@id
        )
      }))
    }
    if (length(x@contents) > 1) {
      cli::cli_abort("Databricks models only accept a single text input.")
    }
    content <- as_json(provider, x@contents[[1]])
    list(list(role = "user", content = content))
  } else if (x@role == "assistant") {
    is_tool <- map_lgl(x@contents, is_tool_request)
    if (any(is_tool)) {
      list(list(
        role = "assistant",
        tool_calls = as_json(provider, x@contents[is_tool])
      ))
    } else {
      # We should be able to assume that there is only one content item here.
      content <- as_json(provider, x@contents[[1]])
      list(list(role = "assistant", content = content))
    }
  } else {
    cli::cli_abort("Unknown role {turn@role}", .internal = TRUE)
  }
}

method(as_json, list(ProviderDatabricks, ContentText)) <- function(
  provider,
  x
) {
  # Databricks only seems to support textual content.
  x@text
}

databricks_workspace <- function() {
  host <- key_get("DATABRICKS_HOST")
  if (!is.null(host) && !grepl("^https?://", host)) {
    host <- paste0("https://", host)
  }
  host
}

databricks_user_agent <- function() {
  user_agent <- paste0("r-ellmer/", utils::packageVersion("ellmer"))
  if (nchar(Sys.getenv("SPARK_CONNECT_USER_AGENT")) != 0) {
    user_agent <- paste(Sys.getenv("SPARK_CONNECT_USER_AGENT"), user_agent)
  }
  user_agent
}

# Try various ways to get Databricks credentials. This implements a subset of
# the "Databricks client unified authentication" model.
default_databricks_credentials <- function(workspace = databricks_workspace()) {
  host <- gsub("https://|/$", "", workspace)

  # Detect viewer-based credentials from Posit Connect.
  if (has_connect_viewer_token(resource = workspace)) {
    return(function() {
      token <- connectcreds::connect_viewer_token(workspace)
      list(Authorization = paste("Bearer", token$access_token))
    })
  }

  # An explicit PAT takes precedence over everything else.
  token <- Sys.getenv("DATABRICKS_TOKEN")
  if (nchar(token)) {
    return(function() list(Authorization = paste("Bearer", token)))
  }

  # Next up are explicit OAuth2 M2M credentials.
  client_id <- Sys.getenv("DATABRICKS_CLIENT_ID")
  client_secret <- Sys.getenv("DATABRICKS_CLIENT_SECRET")
  if (nchar(client_id) && nchar(client_secret)) {
    # M2M credentials use an OAuth client credentials flow. We cache the token
    # so we don't need to perform this flow before each turn.
    client <- oauth_client(
      client_id,
      paste0("https://", host, "/oidc/v1/token"),
      secret = client_secret,
      auth = "header",
      name = "ellmer-databricks-m2m"
    )
    return(function() {
      token <- oauth_token_cached(
        client,
        oauth_flow_client_credentials,
        # The "all-apis" scope translates to "everything this service principal
        # has access to", not "all Databricks APIs".
        flow_params = list(scope = "all-apis"),
        # Don't use the cached token when testing.
        reauth = is_testing()
      )
      list(Authorization = paste("Bearer", token$access_token))
    })
  }

  # Check for Workbench-provided credentials.
  cfg_file <- Sys.getenv("DATABRICKS_CONFIG_FILE")
  if (grepl("posit-workbench", cfg_file, fixed = TRUE)) {
    wb_token <- workbench_databricks_token(host, cfg_file)
    if (!is.null(wb_token)) {
      return(function() {
        # Ensure we get an up-to-date token.
        token <- workbench_databricks_token(host, cfg_file)
        list(Authorization = paste("Bearer", token))
      })
    }
  }

  # When on desktop, try using the Databricks CLI for auth.
  cli_path <- Sys.getenv("DATABRICKS_CLI_PATH", "databricks")
  if (!is_hosted_session() && nchar(Sys.which(cli_path)) != 0) {
    token <- databricks_cli_token(cli_path, host)
    if (!is.null(token)) {
      return(function() {
        # Ensure we get an up-to-date token.
        token <- databricks_cli_token(cli_path, host)
        list(Authorization = paste("Bearer", token))
      })
    }
  }

  # Fall back to OAuth U2M, masquerading as the Databricks CLI. Again, this
  # only works on desktop.
  if (is_interactive() && !is_hosted_session()) {
    # U2M credentials use an OAuth authorization code flow. We cache the token
    # so we don't need to perform this flow before each turn.
    client <- oauth_client(
      "databricks-cli",
      paste0("https://", host, "/oidc/v1/token"),
      auth = "body",
      name = "ellmer-databricks-u2m"
    )
    return(function() {
      token <- oauth_token_cached(
        client,
        oauth_flow_auth_code,
        flow_params = list(
          auth_url = paste0("https://", host, "/oidc/v1/authorize"),
          # The "all-apis" scope translates to "everything this user has access
          # to", not "all Databricks APIs".
          scope = "all-apis offline_access",
          # This is the registered redirect URI for the Databricks CLI.
          redirect_uri = "http://localhost:8020"
        )
      )
      list(Authorization = paste("Bearer", token$access_token))
    })
  }

  if (is_testing()) {
    testthat::skip("no Databricks credentials available")
  }

  cli::cli_abort("No Databricks credentials are available.")
}

# Try to determine whether we can redirect the user's browser to a server on
# localhost, which isn't possible if we are running on a hosted platform.
#
# This is based on the strategy pioneered by the {gargle} package and {httr2}.
is_hosted_session <- function() {
  # If RStudio Server or Posit Workbench is running locally (which is possible,
  # though unusual), it's not acting as a hosted environment.
  Sys.getenv("RSTUDIO_PROGRAM_MODE") == "server" &&
    !grepl("localhost", Sys.getenv("RSTUDIO_HTTP_REFERER"), fixed = TRUE)
}

databricks_cli_token <- function(cli_path, host) {
  output <- suppressWarnings(
    system2(
      cli_path,
      c("auth", "token", "--host", host),
      stdout = TRUE,
      stderr = TRUE
    )
  )
  output <- paste(output, collapse = "\n")
  # If we don't get an error message, try to extract the token from the JSON-
  # formatted output.
  if (grepl("access_token", output, fixed = TRUE)) {
    token <- gsub(".*access_token\":\\s?\"([^\"]+).*", "\\1", output)
    return(token)
  }
  NULL
}

# Reads Posit Workbench-managed Databricks credentials from a
# $DATABRICKS_CONFIG_FILE. The generated file will look as follows:
#
# [workbench]
# host = some-host
# token = some-token
workbench_databricks_token <- function(host, cfg_file) {
  cfg <- readLines(cfg_file)
  # We don't attempt a full parse of the INI syntax supported by Databricks
  # config files, instead relying on the fact that this particular file will
  # always contain only one section.
  if (!any(grepl(host, cfg, fixed = TRUE))) {
    # The configuration doesn't actually apply to this host.
    return(NULL)
  }
  line <- grepl("token = ", cfg, fixed = TRUE)
  token <- gsub("token = ", "", cfg[line])
  if (nchar(token) == 0) {
    return(NULL)
  }
  token
}
