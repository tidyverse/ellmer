#' @include provider.R
#' @include content.R
#' @include turns.R
#' @include tools-def.R
NULL

#' Create a chatbot that speaks to the Snowflake Cortex Analyst
#'
#' @description
#' Chat with the LLM-powered [Snowflake Cortex
#' Analyst](https://docs.snowflake.com/en/user-guide/snowflake-cortex/cortex-analyst).
#'
#' ## Authentication
#'
#' `chat_cortex_analyst()` picks up the following ambient Snowflake credentials:
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
#'
#' Unlike most comparable model APIs, Cortex does not take a system prompt.
#' Instead, the caller must provide a "semantic model" describing available
#' tables, their meaning, and verified queries that can be run against them as a
#' starting point. The semantic model can be passed as a YAML string or via
#' reference to an existing file in a Snowflake Stage.
#'
#' Note that Cortex does not support multi-turn, so it will not remember
#' previous messages. Nor does it support registering tools, and attempting to
#' do so will result in an error.
#'
#' See [chat_snowflake()] to chat with more general-purpose models hosted on
#' Snowflake.
#'
#' @param account A Snowflake [account identifier](https://docs.snowflake.com/en/user-guide/admin-account-identifier),
#'   e.g. `"testorg-test_account"`. Defaults to the value of the
#'   `SNOWFLAKE_ACCOUNT` environment variable.
#' @param credentials A list of authentication headers to pass into
#'   [`httr2::req_headers()`], a function that returns them when called, or
#'   `NULL`, the default, to use ambient credentials.
#' @param model_spec A semantic model specification, or `NULL` when
#'   using `model_file` instead.
#' @param model_file Path to a semantic model file stored in a Snowflake Stage,
#'   or `NULL` when using `model_spec` instead.
#' @inheritParams chat_openai
#' @inherit chat_openai return
#' @family chatbots
#' @examplesIf has_credentials("cortex")
#' chat <- chat_cortex_analyst(
#'   model_file = "@my_db.my_schema.my_stage/model.yaml"
#' )
#' chat$chat("What questions can I ask?")
#' @export
chat_cortex_analyst <- function(
  account = snowflake_account(),
  credentials = NULL,
  model_spec = NULL,
  model_file = NULL,
  api_args = list(),
  echo = c("none", "output", "all")
) {
  check_string(account, allow_empty = FALSE)
  check_string(model_spec, allow_empty = FALSE, allow_null = TRUE)
  check_string(model_file, allow_empty = FALSE, allow_null = TRUE)
  check_exclusive(model_spec, model_file)
  echo <- check_echo(echo)

  if (is_list(credentials)) {
    static_credentials <- force(credentials)
    credentials <- function(account) static_credentials
  }
  check_function(credentials, allow_null = TRUE)
  credentials <- credentials %||% default_snowflake_credentials(account)

  provider <- ProviderSnowflakeCortexAnalyst(
    name = "Snowflake/CortexAnalyst",
    account = account,
    credentials = credentials,
    model_spec = model_spec,
    model_file = model_file,
    extra_args = api_args
  )

  Chat$new(provider = provider, echo = echo)
}

ProviderSnowflakeCortexAnalyst <- new_class(
  "ProviderSnowflakeCortexAnalyst",
  parent = Provider,
  constructor = function(
    name,
    account,
    credentials,
    model_spec = NULL,
    model_file = NULL,
    extra_args = list()
  ) {
    base_url <- snowflake_url(account)
    extra_args <- compact(list2(
      semantic_model = model_spec,
      semantic_model_file = model_file,
      !!!extra_args
    ))
    new_object(
      Provider(
        name = name,
        base_url = base_url,
        extra_args = extra_args,
        model = ""
      ),
      account = account,
      credentials = credentials
    )
  },
  properties = list(
    account = prop_string(),
    credentials = class_function,
    extra_args = class_list
  )
)

provider_cortex_test <- function(..., credentials = function(account) list()) {
  ProviderSnowflakeCortexAnalyst(
    name = "Cortex",
    account = "testorg-test_account",
    credentials = credentials,
    ...
  )
}

# See: https://docs.snowflake.com/en/developer-guide/snowflake-rest-api/reference/cortex-analyst
#      https://docs.snowflake.com/en/user-guide/snowflake-cortex/cortex-analyst/tutorials/tutorial-1#step-3-create-a-streamlit-app-to-talk-to-your-data-through-cortex-analyst
method(chat_request, ProviderSnowflakeCortexAnalyst) <- function(
  provider,
  stream = TRUE,
  turns = list(),
  tools = list(),
  type = NULL
) {
  if (length(tools) != 0) {
    cli::cli_abort("Tools are not supported by Cortex.")
  }
  if (!is.null(type) != 0) {
    cli::cli_abort("Structured data extraction is not supported by Cortex.")
  }

  req <- request(provider@base_url)
  req <- req_url_path_append(req, "/api/v2/cortex/analyst/message")
  req <- ellmer_req_credentials(req, provider@credentials)
  req <- req_retry(req, max_tries = 2)
  req <- ellmer_req_timeout(req, stream)
  req <- ellmer_req_user_agent(req, Sys.getenv("SF_PARTNER"))

  # Snowflake doesn't document the error response format for Cortex Analyst at
  # this time, but empirically errors look like the following:
  #
  # {
  #  "message" : "'@stage' is an invalid stage file URL (correct format: '@my_db.my_schema.my_stage/path/to/file.yaml')",
  #  "code" : null,
  #  "error_code" : "392706",
  #  "request_id" : "929dd0e5-a582-458c-a9d2-c1aed531bb66"
  # }
  req <- req_error(req, body = function(resp) resp_body_json(resp)$message)

  # Cortex does not yet support multi-turn chats.
  turns <- tail(turns, n = 1)
  messages <- as_json(provider, turns)

  body <- list(messages = messages, stream = stream)
  body <- modify_list(body, provider@extra_args)
  req <- req_body_json(req, body)

  req
}

# Cortex -> ellmer --------------------------------------------------------------

method(stream_parse, ProviderSnowflakeCortexAnalyst) <- function(
  provider,
  event
) {
  # While undocumented, Cortex seems to mostly follow OpenAI API conventions for
  # streaming, although Cortex uses a specific JSON payload rather than in-band
  # signalling that the stream has ended.
  if (identical(event$data, '{"status":"done","status_message":"Done"}')) {
    NULL
  } else {
    jsonlite::parse_json(event$data)
  }
}

method(stream_text, ProviderSnowflakeCortexAnalyst) <- function(
  provider,
  event
) {
  if (!is.null(event$status_message)) {
    # Skip status messages like "Interpreting question" or "Generating
    # suggestions".
    ""
  } else if (!is.null(event$text_delta)) {
    event$text_delta
  } else if (!is.null(event$statement_delta)) {
    # Emit a Markdown-formatted code block for SQL queries.
    paste0("\n\n```sql\n", event$statement_delta, "\n```")
  } else if (!is.null(event$suggestions_delta)) {
    # Emit a Markdown-formatted "Suggestions" section.
    if (event$suggestions_delta$index == 0) {
      paste0(
        "\n\n#### Suggestions\n\n- ",
        event$suggestions_delta$suggestion_delta
      )
    } else {
      paste0("\n- ", event$suggestions_delta$suggestion_delta)
    }
  } else {
    cli::cli_abort(
      "Unknown chunk type {.str {event$type}}.",
      .internal = TRUE
    )
  }
}

method(stream_merge_chunks, ProviderSnowflakeCortexAnalyst) <- function(
  provider,
  result,
  chunk
) {
  if (!is.null(chunk$status)) {
    # Skip status messages.
    result
  } else if (is.null(result)) {
    list(cortex_chunk_to_message(chunk))
  } else {
    elt <- cortex_chunk_to_message(chunk)
    idx <- chunk$index + 1
    # This is a new chunk, we don't need to merge.
    if (length(result) < idx) {
      result[[idx]] <- elt
      return(result)
    }
    # Only suggestion-type chunks are emitted piecemeal.
    if (identical(chunk$type, "suggestions")) {
      result[[idx]]$suggestions <- c(
        result[[idx]]$suggestions,
        elt$suggestions
      )
    } else {
      cli::cli_abort(
        "Unmergeable chunk type {.str {chunk$type}}.",
        .internal = TRUE
      )
    }
    result
  }
}

cortex_chunk_to_message <- function(x) {
  if (x$type == "text") {
    list(type = x$type, text = x$text_delta)
  } else if (x$type == "sql") {
    list(type = x$type, statement = x$statement_delta)
  } else if (x$type == "suggestions") {
    list(
      type = x$type,
      suggestions = list(x$suggestions_delta$suggestion_delta)
    )
  } else {
    cli::cli_abort(
      "Unknown chunk type {.str {x$type}}.",
      .internal = TRUE
    )
  }
}

method(value_turn, ProviderSnowflakeCortexAnalyst) <- function(
  provider,
  result,
  has_type = FALSE
) {
  is_streaming <- !is_named(result)
  content <- if (is_streaming) result else result$content

  assistant_turn(
    contents = lapply(content, function(x) {
      if (x$type == "text") {
        if (!has_name(x, "text")) {
          cli::cli_abort("'text'-type content must have a 'text' field.")
        }
        ContentText(x$text)
      } else if (identical(x$type, "suggestions")) {
        if (!has_name(x, "suggestions")) {
          cli::cli_abort(
            "'suggestions'-type content must have a 'suggestions' field."
          )
        }
        ContentSuggestions(unlist(x$suggestions))
      } else if (identical(x$type, "sql")) {
        if (!has_name(x, "statement")) {
          cli::cli_abort("'sql'-type content must have a 'statement' field.")
        }
        ContentSql(x$statement)
      } else {
        cli::cli_abort(
          "Unknown content type {.str {x$type}} in response.",
          .internal = TRUE
        )
      }
    })
  )
}


# ellmer -> Cortex --------------------------------------------------------------

# Cortex supports not only "text" content, but also bespoke "suggestions" and
# "sql" types.

method(as_json, list(ProviderSnowflakeCortexAnalyst, Turn)) <- function(
  provider,
  x
) {
  role <- x@role
  if (role == "assistant") {
    role <- "analyst"
  }
  list(
    role = role,
    content = as_json(provider, x@contents)
  )
}

method(as_json, list(ProviderSnowflakeCortexAnalyst, ContentText)) <- function(
  provider,
  x
) {
  list(type = "text", text = x@text)
}

ContentSuggestions <- new_class(
  "ContentSuggestions",
  parent = Content,
  properties = list(suggestions = class_character)
)

method(
  as_json,
  list(ProviderSnowflakeCortexAnalyst, ContentSuggestions)
) <- function(
  provider,
  x
) {
  list(type = "suggestions", suggestions = as.list(x@suggestions))
}

method(contents_text, ContentSuggestions) <- function(content) {
  # Emit a Markdown-formatted "Suggestions" section as the textual
  # representation.
  paste0(
    c("\n\n#### Suggestions\n", paste0("- ", content@suggestions)),
    collapse = "\n"
  )
}

method(format, ContentSuggestions) <- function(x, ...) {
  items <- x@suggestions
  names(items) <- rep("*", times = length(items))
  paste0(
    c(
      cli::format_inline("{.strong Suggestions:}"),
      cli::format_bullets_raw(items)
    ),
    collapse = "\n"
  )
}

ContentSql <- new_class(
  "ContentSql",
  parent = Content,
  properties = list(statement = prop_string())
)

method(as_json, list(ProviderSnowflakeCortexAnalyst, ContentSql)) <- function(
  provider,
  x
) {
  list(type = "sql", statement = x@statement)
}

method(contents_text, ContentSql) <- function(content) {
  # Emit a Markdown-formatted SQL code block as the textual representation.
  contents_markdown(content)
}

method(contents_markdown, ContentSql) <- function(content) {
  paste0("\n\n```sql\n", content@statement, "\n```")
}

method(contents_html, ContentSql) <- function(content) {
  sprintf('<pre><code>%s</code></pre>\n', content@statement)
}

method(format, ContentSql) <- function(x, ...) {
  cli::format_inline("{.strong SQL:} {.code {x@statement}}")
}

# Credential handling ----------------------------------------------------------

cortex_credentials_exist <- function(...) {
  tryCatch(
    is_list(default_snowflake_credentials(...)),
    error = function(e) FALSE
  )
}

# Reads Posit Workbench-managed Snowflake credentials from a
# $SNOWFLAKE_HOME/connections.toml file, as used by the Snowflake Connector for
# Python implementation. The file will look as follows:
#
# [workbench]
# account = "account-id"
# token = "token"
# authenticator = "oauth"
workbench_snowflake_token <- function(account, sf_home) {
  cfg <- readLines(file.path(sf_home, "connections.toml"))
  # We don't attempt a full parse of the TOML syntax, instead relying on the
  # fact that this file will always contain only one section.
  if (!any(grepl(account, cfg, fixed = TRUE))) {
    # The configuration doesn't actually apply to this account.
    return(NULL)
  }
  line <- grepl("token = ", cfg, fixed = TRUE)
  token <- gsub("token = ", "", cfg[line])
  if (nchar(token) == 0) {
    return(NULL)
  }
  # Drop enclosing quotes.
  gsub("\"", "", token)
}
