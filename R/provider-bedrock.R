#' @include provider.R
#' @include content.R
#' @include turns.R
#' @include tools-def.R
NULL

#' Chat with an AWS bedrock model
#'
#' @description
#' [AWS Bedrock](https://aws.amazon.com/bedrock/) provides a number of chat
#' based models, including those Anthropic's
#' [Claude](https://aws.amazon.com/bedrock/claude/).
#'
#' ## Authentication
#' 
#' Authenthication is handled through \{paws.common\}, so if authenthication
#' does not work for you automatically, you'll need to follow the advice
#' at <https://www.paws-r-sdk.com/#credentials>. In particular, if your
#' org uses AWS SSO, you'll need to run `aws sso login` at the terminal.
#'
#' @param profile AWS profile to use.
#' @inheritParams chat_openai
#' @inherit chat_openai return
#' @family chatbots
#' @export
#' @examples
#' \dontrun{
#' chat <- chat_bedrock()
#' chat$chat("Tell me three jokes about statisticians")
#' }
chat_bedrock <- function(system_prompt = NULL,
                         turns = NULL,
                         model = NULL,
                         profile = NULL,
                         echo = NULL,
                         api_args = NULL,
                         verbose = FALSE) {

  check_installed("paws.common", "AWS authentication")
  cache <- aws_creds_cache(profile)
  credentials <- paws_credentials(profile, cache = cache)

  # Validate api_args if present
  if (!is.null(api_args)) {
    validate_parameters(api_args, model)
  }

  turns <- normalize_turns(turns, system_prompt)
  model <- set_default(model, "us.anthropic.claude-3-5-sonnet-20240620-v1:0")
  echo <- check_echo(echo)

  provider <- ProviderBedrock(
    base_url = "",
    model = model,
    profile = profile,
    region = credentials$region,
    cache = cache,
    api_args = if (is.null(api_args)) list() else api_args,
    verbose = verbose
  )

  Chat$new(provider = provider, turns = turns, echo = echo)
}

ProviderBedrock <- new_class(
  "ProviderBedrock",
  parent = Provider,
  properties = list(
    model = prop_string(),
    profile = prop_string(allow_null = TRUE),
    region = prop_string(),
    cache = class_list,
    api_args = class_list,
    verbose = class_logical
  )
)

validate_parameters <- function(api_args, model) {
  # Check for unsupported parameters in Llama models
  if (grepl("llama", model, ignore.case = TRUE)) {
    if (!is.null(api_args$top_k)) {
      cli::cli_abort("top_k parameter is not supported for Llama models")
    }
    if (!is.null(api_args$stop_sequences)) {
      cli::cli_abort("stop_sequences parameter is not supported for Llama models")
    }
  }
  
  # Validate temperature
  if (!is.null(api_args$temperature) && (api_args$temperature < 0 || api_args$temperature > 1)) {
    cli::cli_abort("temperature must be a numeric value between 0 and 1, inclusive")
  }
  
  # Validate top_p
  if (!is.null(api_args$top_p) && (api_args$top_p < 0 || api_args$top_p > 1)) {
    cli::cli_abort("top_p must be a numeric value between 0 and 1, inclusive")
  }
  
  # Validate top_k
  if (!is.null(api_args$top_k)) {
    if (!is.numeric(api_args$top_k) || api_args$top_k <= 0 || api_args$top_k %% 1 != 0) {
      cli::cli_abort("top_k must be a positive integer")
    }
  }
}

method(chat_request, ProviderBedrock) <- function(provider,
                                                  stream = TRUE,
                                                  turns = list(),
                                                  tools = list(),
                                                  type = NULL,
                                                  extra_args = list()) {

  # Validate parameters if api_args are present
  if (length(provider@api_args) > 0) {
    validate_parameters(provider@api_args, provider@model)
  }

  req <- request(paste0(
    "https://bedrock-runtime.", provider@region, ".amazonaws.com"
  ))
  req <- req_url_path_append(
    req,
    "model",
    provider@model,
    if (stream) "converse-stream" else "converse"
  )
  creds <- paws_credentials(provider@profile, provider@cache)
  req <- req_auth_aws_v4(
    req,
    aws_access_key_id = creds$access_key_id,
    aws_secret_access_key = creds$secret_access_key,
    aws_session_token = creds$session_token
  )

  if (provider@verbose) { 
    cli::cli_h3("AWS Credentials")
    cli::cli_alert_info(paste0("Profile: ", provider@profile, 
    "; Key: ", paste0(creds$access_key_id), 
    "; Secret: ", paste0(substr(creds$secret_access_key, 1, 2), 
                        paste(rep("*", 4), collapse = "")), 
    "; Session: ", creds$session_token, 
    "; Region: ", provider@region)) 
  }

  req <- req_error(req, body = function(resp) {
    body <- resp_body_json(resp)
    body$Message %||% body$message
  })

  if (length(turns) >= 1 && is_system_prompt(turns[[1]])) {
    system <- list(list(text = turns[[1]]@text))
  } else {
    system <- NULL
  }

  messages <- compact(as_json(provider, turns))

  if (!is.null(type)) {
    tool_def <- ToolDef(
      fun = function(...) {},
      name = "structured_tool_call__",
      description = "Extract structured data",
      arguments = type_object(data = type)
    )
    tools[[tool_def@name]] <- tool_def
    tool_choice <- list(tool = list(name = tool_def@name))
    stream <- FALSE
  } else {
    tool_choice <- NULL
  }

  if (length(tools) > 0) {
    tools <- as_json(provider, unname(tools))
    toolConfig <- compact(list(tools = tools, tool_choice = tool_choice))
  } else {
    toolConfig <- NULL
  }

  # Build request body
  body <- list(
    messages = messages,
    system = system,
    toolConfig = toolConfig
  )

  # Add inference configuration from api_args if present
  if (length(provider@api_args) > 0) {
    inference_config <- list()
    
    # Convert snake_case parameters to camelCase for API
    if (!is.null(provider@api_args$max_tokens)) {
      inference_config$maxTokens <- provider@api_args$max_tokens
    }
    if (!is.null(provider@api_args$temperature)) {
      inference_config$temperature <- provider@api_args$temperature
    }
    if (!is.null(provider@api_args$top_p)) {
      inference_config$topP <- provider@api_args$top_p
    }
    if (!is.null(provider@api_args$top_k)) {
      inference_config$topK <- provider@api_args$top_k
    }
    if (!is.null(provider@api_args$stop_sequences)) {
      inference_config$stopSequences <- provider@api_args$stop_sequences
    }
    
    # Only add inferenceConfig if we have parameters
    if (length(inference_config) > 0) {
      body$inferenceConfig <- inference_config
    }
  }

  req <- req_body_json(req, body)

  if (provider@verbose) {
    cli::cli_h3("Request Body")
    cat(jsonlite::toJSON(body, auto_unbox = TRUE, pretty = TRUE), "\n")
    # Use standard header redaction with logical value
    req <- httr2::req_verbose(req)
  }

  return(req)
}

method(chat_resp_stream, ProviderBedrock) <- function(provider, resp) {
  if (provider@verbose) {
    cli::cli_h3("Response Stream")
  }
  resp_stream_aws(resp)
}

# Bedrock -> ellmer -------------------------------------------------------------

method(stream_parse, ProviderBedrock) <- function(provider, event) {
  if (is.null(event)) {
    return()
  }

  body <- event$body
  body$event_type <- event$headers$`:event-type`
  body$p <- NULL # padding? Looks like: "p": "abcdefghijklmnopqrstuvwxyzABCDEFGHIJ",

  if (provider@verbose) {
    cli::cli_h3("Response Chunk")
    cat(jsonlite::toJSON(body, auto_unbox = TRUE, pretty = TRUE), "\n")
  }

  body
}

method(stream_text, ProviderBedrock) <- function(provider, event) {
  if (event$event_type == "contentBlockDelta") {
    event$delta$text
  }
}

method(stream_merge_chunks, ProviderBedrock) <- function(provider, result, chunk) {
  i <- chunk$contentBlockIndex + 1

  if (chunk$event_type == "messageStart") {
    result <- list(role = chunk$role, content = list())
  } else if (chunk$event_type == "contentBlockStart") {
    result$content[[i]] <- list(toolUse = chunk$start$toolUse)
  } else if (chunk$event_type == "contentBlockDelta") {
    if (has_name(chunk$delta, "text")) {
      if (i > length(result$content)) {
        result$content[[i]] <- list(text = chunk$delta$text)
      } else {
        paste(result$content[[i]]$text) <- chunk$delta$text
      }
    } else if (has_name(chunk$delta, "toolUse")) {
      paste(result$content[[i]]$toolUse$input) <- chunk$delta$toolUse$input
    } else {
      cli::cli_abort("Unknown chunk type {names(chunk$delta)}", .internal = TRUE)
    }
  } else if (chunk$event_type == "contentBlockStop") {
    if (has_name(result$content[[i]], "toolUse")) {
      input <- result$content[[i]]$toolUse$input
      if (input == "") {
        result$content[[i]]$toolUse$input <- set_names(list())
      } else {
        result$content[[i]]$toolUse$input <- jsonlite::parse_json(input)
      }
    }
  } else if (chunk$event_type == "messageStop") {
    # match structure of non-streaming
    result <- list(
      output = list(
        message = result
      )
    )
  } else if (chunk$event_type == "metadata") {
    result$usage <- chunk$usage
    result$metrics <- chunk$metrics
  } else {
    cli::cli_inform(c("!" = "Unknown chunk type {.str {event_type}}."))
  }

  result
}

method(value_turn, ProviderBedrock) <- function(provider, result, has_type = FALSE) {
  # Print response if verbose mode is enabled
  if (provider@verbose) {
    cli::cli_h3("Response Body")
    cat(jsonlite::toJSON(result, auto_unbox = TRUE, pretty = TRUE), "\n")
  }

  contents <- lapply(result$output$message$content, function(content) {
    if (has_name(content, "text")) {
      ContentText(content$text)
    } else if (has_name(content, "toolUse")) {
      if (has_type) {
        ContentJson(content$toolUse$input$data)
      } else {
        ContentToolRequest(
          name = content$toolUse$name,
          arguments = content$toolUse$input,
          id = content$toolUse$toolUseId
        )
      }
    } else {
      cli::cli_abort(
        "Unknown content type {.str {names(content)}}.",
        .internal = TRUE
      )
    }
  })

  tokens <- c(result$usage$inputTokens, result$usage$outputTokens)
  tokens_log("Bedrock", tokens)

  Turn(result$output$message$role, contents, json = result, tokens = tokens)
}

# ellmer -> Bedrock -------------------------------------------------------------

# https://docs.aws.amazon.com/bedrock/latest/APIReference/API_runtime_ContentBlock.html
method(as_json, list(ProviderBedrock, Turn)) <- function(provider, x) {
  if (x@role == "system") {
    # bedrock passes system prompt as separate arg
    NULL
  } else if (x@role %in% c("user", "assistant")) {
    list(role = x@role, content = as_json(provider, x@contents))
  } else {
    cli::cli_abort("Unknown role {turn@role}", .internal = TRUE)
  }
}

method(as_json, list(ProviderBedrock, ContentText)) <- function(provider, x) {
  list(text = x@text)
}

method(as_json, list(ProviderBedrock, ContentImageRemote)) <- function(provider, x) {
  cli::cli_abort("Bedrock doesn't support remote images")
}

# https://docs.aws.amazon.com/bedrock/latest/APIReference/API_runtime_ImageBlock.html
method(as_json, list(ProviderBedrock, ContentImageInline)) <- function(provider, x) {
  type <- switch(x@type,
    "image/png" = "png",
    "image/gif" = "gif",
    "image/jpeg" = "jpeg",
    "image/webp" = "webp",
    cli::cli_abort("Image type {content@type} is not supported by bedrock")
  )

  list(
    image = list(
      format = type,
      source = list(bytes = x@data)
    )
  )
}

# https://docs.aws.amazon.com/bedrock/latest/APIReference/API_runtime_ToolUseBlock.html
method(as_json, list(ProviderBedrock, ContentToolRequest)) <- function(provider, x) {
  list(
    toolUse = list(
      toolUseId = x@id,
      name = x@name,
      input = x@arguments
    )
  )
}

# https://docs.aws.amazon.com/bedrock/latest/APIReference/API_runtime_ToolResultBlock.html
method(as_json, list(ProviderBedrock, ContentToolResult)) <- function(provider, x) {
  list(
    toolResult = list(
      toolUseId = x@id,
      content = list(list(text = tool_string(x))),
      status = if (tool_errored(x)) "error" else "success"
    )
  )
}

method(as_json, list(ProviderBedrock, ToolDef)) <- function(provider, x) {
  list(
    toolSpec = list(
      name = x@name,
      description = x@description,
      inputSchema = list(json = compact(as_json(provider, x@arguments)))
    )
  )
}

# Helpers ----------------------------------------------------------------

paws_credentials <- function(profile, cache = aws_creds_cache(profile),
                             reauth = FALSE) {
  creds <- cache$get()
  if (reauth || is.null(creds) || creds$expiration < Sys.time()) {
    cache$clear()
    try_fetch(
      creds <- locate_aws_credentials(profile),
      error = function(cnd) {
        if (is_testing()) {
          testthat::skip("Failed to locate AWS credentials")
        }
        cli::cli_abort("No IAM credentials found.", parent = cnd)
      }
    )
    cache$set(creds)
  }
  creds
}

# Wrapper for paws.common::locate_credentials() so we can mock it in tests.
locate_aws_credentials <- function(profile) {
  paws.common::locate_credentials(profile)
}

aws_creds_cache <- function(profile) {
  credentials_cache(key = hash(c("aws", profile)))
}
