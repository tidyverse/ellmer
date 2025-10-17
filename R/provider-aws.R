#' @include provider.R
#' @include content.R
#' @include turns.R
#' @include tools-def.R
NULL

#' Chat with an AWS bedrock model
#'
#' @description
#' [AWS Bedrock](https://aws.amazon.com/bedrock/) provides a number of
#' language models, including those from Anthropic's
#' [Claude](https://aws.amazon.com/bedrock/claude/), using the Bedrock
#' [Converse API](https://docs.aws.amazon.com/bedrock/latest/APIReference/API_runtime_Converse.html).
#'
#' ## Authentication
#'
#' Authentication is handled through \{paws.common\}, so if authentication
#' does not work for you automatically, you'll need to follow the advice
#' at <https://www.paws-r-sdk.com/#credentials>. In particular, if your
#' org uses AWS SSO, you'll need to run `aws sso login` at the terminal.
#'
#' @param profile AWS profile to use.
#' @param model `r param_model("anthropic.claude-3-5-sonnet-20240620-v1:0", "models_aws_bedrock")`.
#'
#'   While ellmer provides a default model, there's no guarantee that you'll
#'   have access to it, so you'll need to specify a model that you can.
#'   If you're using [cross-region inference](https://aws.amazon.com/blogs/machine-learning/getting-started-with-cross-region-inference-in-amazon-bedrock/),
#'   you'll need to use the inference profile ID, e.g.
#'   `model="us.anthropic.claude-3-5-sonnet-20240620-v1:0"`.
#' @param params Common model parameters, usually created by [params()].
#' @param api_args Named list of arbitrary extra arguments appended to the body
#'   of every chat API call. Some useful arguments include:
#'
#'   ```R
#'   api_args = list(
#'     inferenceConfig = list(
#'       maxTokens = 100,
#'       temperature = 0.7,
#'       topP = 0.9,
#'       topK = 20
#'     )
#'   )
#'   ```
#' @inheritParams chat_openai
#' @inherit chat_openai return
#' @family chatbots
#' @export
#' @examples
#' \dontrun{
#' # Basic usage
#' chat <- chat_aws_bedrock()
#' chat$chat("Tell me three jokes about statisticians")
#' }
chat_aws_bedrock <- function(
  system_prompt = NULL,
  base_url = NULL,
  model = NULL,
  profile = NULL,
  params = NULL,
  api_args = list(),
  api_headers = character(),
  echo = NULL
) {
  check_installed("paws.common", "AWS authentication")
  check_string(base_url, allow_null = TRUE)
  base_url <- base_url %||%
    \(x) sprintf("https://bedrock-runtime.%s.amazonaws.com", x)
  echo <- check_echo(echo)

  params <- params %||% params()

  provider <- provider_aws_bedrock(
    base_url = base_url,
    model = model,
    profile = profile,
    params = params,
    extra_args = api_args,
    extra_headers = api_headers
  )
  Chat$new(provider = provider, system_prompt = system_prompt, echo = echo)
}


#' @export
#' @rdname chat_aws_bedrock
models_aws_bedrock <- function(profile = NULL, base_url = NULL) {
  check_string(base_url, allow_null = TRUE)
  base_url <- base_url %||% \(x) sprintf("https://bedrock.%s.amazonaws.com", x)

  provider <- provider_aws_bedrock(
    base_url = base_url,
    model = "",
    profile = profile,
  )
  req <- base_request(provider)
  req <- req_url_path_append(req, "foundation-models")

  resp <- req_perform(req)
  json <- resp_body_json(resp)
  models <- json$modelSummaries

  df <- data.frame(
    id = map_chr(models, "[[", "modelId"),
    name = map_chr(models, "[[", "modelName"),
    provider = map_chr(models, "[[", "providerName")
  )
  df <- cbind(df, match_prices("AWS/Bedrock", df$id))
  df
}

provider_aws_bedrock <- function(
  base_url,
  model = "",
  profile = NULL,
  params = list(),
  extra_args = list(),
  extra_headers = character()
) {
  cache <- aws_creds_cache(profile)
  credentials <- paws_credentials(profile, cache = cache)

  if (is.function(base_url)) {
    base_url <- base_url(credentials$region)
  }

  model <- set_default(model, "anthropic.claude-3-5-sonnet-20240620-v1:0")

  ProviderAWSBedrock(
    name = "AWS/Bedrock",
    base_url = base_url,
    model = model,
    profile = profile,
    region = credentials$region,
    cache = cache,
    params = params,
    extra_args = extra_args,
    extra_headers = extra_headers
  )
}

ProviderAWSBedrock <- new_class(
  "ProviderAWSBedrock",
  parent = Provider,
  properties = list(
    profile = prop_string(allow_null = TRUE),
    region = prop_string(),
    cache = class_list
  )
)

method(base_request, ProviderAWSBedrock) <- function(provider) {
  creds <- paws_credentials(provider@profile, provider@cache)

  req <- request(provider@base_url)
  req <- req_auth_aws_v4(
    req,
    aws_access_key_id = creds$access_key_id,
    aws_secret_access_key = creds$secret_access_key,
    aws_session_token = creds$session_token
  )
  req <- ellmer_req_robustify(req)
  req <- ellmer_req_user_agent(req)
  req <- base_request_error(provider, req)
  req
}

method(base_request_error, ProviderAWSBedrock) <- function(provider, req) {
  req_error(req, body = function(resp) {
    body <- resp_body_json(resp)
    body$Message %||% body$message
  })
}

method(chat_params, ProviderAWSBedrock) <- function(provider, params) {
  # https://docs.aws.amazon.com/bedrock/latest/APIReference/API_runtime_InferenceConfiguration.html
  standardise_params(
    params,
    c(
      temperature = "temperature",
      topP = "top_p",
      maxTokens = "max_tokens",
      stopSequences = "stop_sequences"
    )
  )
}

method(chat_request, ProviderAWSBedrock) <- function(
  provider,
  stream = TRUE,
  turns = list(),
  tools = list(),
  type = NULL
) {
  req <- base_request(provider)
  req <- req_url_path_append(
    req,
    "model",
    provider@model,
    if (stream) "converse-stream" else "converse"
  )

  if (length(turns) >= 1 && is_system_prompt(turns[[1]])) {
    system <- list(list(text = turns[[1]]@text))
  } else {
    system <- NULL
  }

  messages <- compact(as_json(provider, turns))

  if (!is.null(type)) {
    tool_def <- ToolDef(
      function(...) {},
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

  # Merge params into inferenceConfig, giving precedence to manual api_args
  params <- chat_params(provider, provider@params)

  extra_args <- provider@extra_args
  extra_args$inferenceConfig <- modify_list(params, extra_args$inferenceConfig)

  # https://docs.aws.amazon.com/bedrock/latest/APIReference/API_runtime_Converse.html
  body <- compact(list2(
    messages = messages,
    system = system,
    toolConfig = toolConfig,
    !!!extra_args
  ))

  req <- req_body_json(req, body)
  req <- req_headers(req, !!!provider@extra_headers)

  req
}

method(chat_resp_stream, ProviderAWSBedrock) <- function(provider, resp) {
  resp_stream_aws(resp)
}

# Bedrock -> ellmer -------------------------------------------------------------

method(stream_parse, ProviderAWSBedrock) <- function(provider, event) {
  if (is.null(event)) {
    return()
  }

  body <- event$body
  body$event_type <- event$headers$`:event-type`
  body$p <- NULL # padding? Looks like: "p": "abcdefghijklmnopqrstuvwxyzABCDEFGHIJ",

  body
}

method(stream_text, ProviderAWSBedrock) <- function(provider, event) {
  if (event$event_type == "contentBlockDelta") {
    event$delta$text
  }
}

method(stream_merge_chunks, ProviderAWSBedrock) <- function(
  provider,
  result,
  chunk
) {
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
      cli::cli_abort(
        "Unknown chunk type {names(chunk$delta)}",
        .internal = TRUE
      )
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

method(value_tokens, ProviderAWSBedrock) <- function(provider, json) {
  tokens(
    input = json$usage$inputTokens,
    output = json$usage$outputTokens,
  )
}

method(value_turn, ProviderAWSBedrock) <- function(
  provider,
  result,
  has_type = FALSE
) {
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

  tokens <- value_tokens(provider, result)
  assistant_turn(contents, json = result, tokens = unlist(tokens))
}

# ellmer -> Bedrock -------------------------------------------------------------

# https://docs.aws.amazon.com/bedrock/latest/APIReference/API_runtime_ContentBlock.html
method(as_json, list(ProviderAWSBedrock, Turn)) <- function(provider, x, ...) {
  if (x@role == "system") {
    # bedrock passes system prompt as separate arg
    NULL
  } else if (x@role %in% c("user", "assistant")) {
    list(role = x@role, content = as_json(provider, x@contents, ...))
  } else {
    cli::cli_abort("Unknown role {turn@role}", .internal = TRUE)
  }
}

method(as_json, list(ProviderAWSBedrock, ContentText)) <- function(
  provider,
  x,
  ...
) {
  if (is_whitespace(x@text)) {
    list(text = "[empty string]")
  } else {
    list(text = x@text)
  }
}

method(as_json, list(ProviderAWSBedrock, ContentImageRemote)) <- function(
  provider,
  x,
  ...
) {
  cli::cli_abort("Bedrock doesn't support remote images")
}

# https://docs.aws.amazon.com/bedrock/latest/APIReference/API_runtime_ImageBlock.html
method(as_json, list(ProviderAWSBedrock, ContentImageInline)) <- function(
  provider,
  x,
  ...
) {
  type <- switch(
    x@type,
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

# https://docs.aws.amazon.com/bedrock/latest/APIReference/API_runtime_DocumentBlock.html
method(as_json, list(ProviderAWSBedrock, ContentPDF)) <- function(
  provider,
  x,
  ...
) {
  list(
    document = list(
      #> This field is vulnerable to prompt injections, because the model
      #> might inadvertently interpret it as instructions. Therefore, we
      #> that you specify a neutral name.
      name = bedrock_document_name(),
      format = "pdf",
      source = list(bytes = x@data)
    )
  )
}

# https://docs.aws.amazon.com/bedrock/latest/APIReference/API_runtime_ToolUseBlock.html
method(as_json, list(ProviderAWSBedrock, ContentToolRequest)) <- function(
  provider,
  x,
  ...
) {
  list(
    toolUse = list(
      toolUseId = x@id,
      name = x@name,
      input = x@arguments
    )
  )
}

# https://docs.aws.amazon.com/bedrock/latest/APIReference/API_runtime_ToolResultBlock.html
method(as_json, list(ProviderAWSBedrock, ContentToolResult)) <- function(
  provider,
  x,
  ...
) {
  list(
    toolResult = list(
      toolUseId = x@request@id,
      content = list(list(text = tool_string(x))),
      status = if (tool_errored(x)) "error" else "success"
    )
  )
}

method(as_json, list(ProviderAWSBedrock, ToolDef)) <- function(
  provider,
  x,
  ...
) {
  list(
    toolSpec = list(
      name = x@name,
      description = x@description,
      inputSchema = list(json = compact(as_json(provider, x@arguments, ...)))
    )
  )
}

# Helpers ----------------------------------------------------------------

paws_credentials <- function(
  profile,
  cache = aws_creds_cache(profile),
  reauth = FALSE
) {
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

bedrock_document_name <- local({
  i <- 1
  function() {
    i <<- i + 1
    paste0("document-", i)
  }
})
