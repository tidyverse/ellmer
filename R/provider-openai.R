#' @include provider.R
#' @include content.R
#' @include turns.R
#' @include tools-def.R
NULL

#' Chat with an OpenAI model
#'
#' @description
#' [OpenAI](https://openai.com/) provides a number of chat-based models,
#' mostly under the [ChatGPT](https://chat.openai.com/) brand.
#' Note that a ChatGPT Plus membership does not grant access to the API.
#' You will need to sign up for a developer account (and pay for it) at the
#' [developer platform](https://platform.openai.com).
#'
#' For authentication, we recommend saving your
#' [API key](https://platform.openai.com/account/api-keys) to
#' the `OPENAI_API_KEY` environment variable in your `.Renviron` file.
#' You can easily edit this file by calling `usethis::edit_r_environ()`.
#'
#' @param system_prompt A system prompt to set the behavior of the assistant.
#' @param turns A list of [Turn]s to start the chat with (i.e., continuing a
#'   previous conversation). If not provided, the conversation begins from
#'   scratch.
#' @param base_url The base URL to the endpoint; the default uses OpenAI.
#' @param api_key The API key to use for authentication. You generally should
#'   not supply this directly, but instead set the `OPENAI_API_KEY` environment
#'   variable.
#' @param model The model to use for the chat. The default, `NULL`, will pick
#'   a reasonable default, and tell you about. We strongly recommend explicitly
#'   choosing a model for all but the most casual use.
#' @param seed Optional integer seed that ChatGPT uses to try and make output
#'   more reproducible.
#' @param api_args Named list of arbitrary extra arguments appended to the body
#'   of every chat API call.
#' @param echo One of the following options:
#'   * `none`: don't emit any output (default when running in a function).
#'   * `text`: echo text output as it streams in (default when running at
#'     the console).
#'   * `all`: echo all input and output.
#'
#'   Note this only affects the `chat()` method.
#' @family chatbots
#' @export
#' @returns A [Chat] object.
#' @examplesIf has_credentials("openai")
#' chat <- chat_openai()
#' chat$chat("
#'   What is the difference between a tibble and a data frame?
#'   Answer with a bulleted list
#' ")
#'
#' chat$chat("Tell me three funny jokes about statistcians")
chat_openai <- function(system_prompt = NULL,
                            turns = NULL,
                            base_url = "https://api.openai.com/v1",
                            api_key = openai_key(),
                            model = NULL,
                            seed = NULL,
                            api_args = list(),
                            echo = c("none", "text", "all")) {
  turns <- normalize_turns(turns, system_prompt)
  model <- set_default(model, "gpt-4o")
  echo <- check_echo(echo)

  if (is_testing() && is.null(seed)) {
    seed <- seed %||% 1014
  }

  provider <- ProviderOpenAI(
    base_url = base_url,
    model = model,
    seed = seed,
    extra_args = api_args,
    api_key = api_key
  )
  Chat$new(provider = provider, turns = turns, echo = echo)
}

ProviderOpenAI <- new_class(
  "ProviderOpenAI",
  parent = Provider,
  properties = list(
    api_key = prop_string(),
    model = prop_string(),
    seed = prop_number_whole(allow_null = TRUE)
  )
)

openai_key_exists <- function() {
  key_exists("OPENAI_API_KEY")
}

openai_key <- function() {
  key_get("OPENAI_API_KEY")
}

method(base_request, ProviderOpenAI) <- function(provider) {
  req <- request(provider@base_url)
  req <- req_auth_bearer_token(req, provider@api_key)
  req <- req_retry(req, max_tries = 2)
  req <- ellmer_req_timeout(req, stream)

  req <- req_error(req, body = function(resp) {
    if (resp_content_type(resp) == "application/json") {
      resp_body_json(resp)$error$message
    } else if (resp_content_type(resp) == "text/plain") {
      resp_body_string(resp)
    }
  })

  req
}

# https://platform.openai.com/docs/api-reference/chat/create
method(chat_request, ProviderOpenAI) <- function(provider,
                                                 stream = TRUE,
                                                 turns = list(),
                                                 tools = list(),
                                                 type = NULL) {

  req <- base_request(provider)
  req <- req_url_path_append(req, "/chat/completions")

  body <- chat_body(provider,
    stream = stream,
    turns = turns,
    tools = tools,
    type = type
  )
  req <- req_body_json(req, body)

  req
}

method(chat_body, ProviderOpenAI) <- function(provider,
                                             stream = TRUE,
                                             turns = list(),
                                             tools = list(),
                                             type = NULL) {

  messages <- compact(unlist(as_json(provider, turns), recursive = FALSE))
  tools <- as_json(provider, unname(tools))

  if (!is.null(type)) {
    response_format <- list(
      type = "json_schema",
      json_schema = list(
        name = "structured_data",
        schema = as_json(provider, type),
        strict = TRUE
      )
    )
  } else {
    response_format <- NULL
  }

  body <- compact(list(
    messages = messages,
    model = provider@model,
    seed = provider@seed,
    stream = stream,
    stream_options = if (stream) list(include_usage = TRUE),
    tools = tools,
    response_format = response_format
  ))
  body <- utils::modifyList(body, provider@extra_args)

  body
}

# Batched requests -------------------------------------------------------------

method(has_batch_support, ProviderOpenAI) <- function(provider) {
  TRUE
}

# https://platform.openai.com/docs/api-reference/batch
method(batch_submit, ProviderOpenAI) <- function(provider, turns, type = NULL) {
  path <- withr::local_tempfile()

  # First put the requests in a file
  # https://platform.openai.com/docs/api-reference/batch/request-input
  requests <- map(seq_along(turns), function(i) {
    body <- chat_body(provider, stream = FALSE, turns = turns[[i]], type = type)

    list(
      custom_id = paste0("chat-", i),
      method = "POST",
      url = "/v1/chat/completions",
      body = body
    )
  })
  json <- map_chr(requests, jsonlite::toJSON, auto_unbox = TRUE)
  writeLines(json, path)
  # Then upload it
  uploaded <- openai_upload(provider, path)

  # Now we can submit the
  req <- base_request(provider)
  req <- req_url_path_append(req, "/batches")
  req <- req_body_json(req, list(
    input_file_id = uploaded$id,
    endpoint = "/v1/chat/completions",
    completion_window = "24h"
  ))

  resp <- req_perform(req)
  resp_body_json(resp)
}

# https://platform.openai.com/docs/api-reference/batch/retrieve
openai_upload <- function(provider, path, purpose = "batch") {
  req <- base_request(provider)
  req <- req_url_path_append(req, "/files")
  req <- req_body_multipart(req, purpose = purpose, file = curl::form_file(path))
  req <- req_progress(req, "up")

  resp <- req_perform(req)
  resp_body_json(resp)
}

# https://docs.anthropic.com/en/api/retrieving-message-batches
method(batch_poll, ProviderOpenAI) <- function(provider, batch) {
  req <- base_request(provider)
  req <- req_url_path_append(req, "/batches/", batch$id)

  resp <- req_perform(req)
  resp_body_json(resp)
}

method(batch_info, ProviderOpenAI) <- function(provider, batch) {
  counts <- batch$request_counts

  list(
    working = batch$status != "completed",
    counts = list(
      processing = counts$total - counts$completed,
      succeeded = counts$completed,
      failed = counts$failed
    )
  )
}

# https://docs.anthropic.com/en/api/retrieving-message-batch-results
method(batch_retrieve, ProviderOpenAI) <- function(provider, batch) {
  path <- withr::local_tempfile()

  req <- base_request(provider)
  req <- req_url_path_append(req, "/files/", batch$output_file_id, "/content")
  req <- req_progress(req, "down")
  resp <- req_perform(req, path = path)

  lines <- readLines(path, warn = FALSE)
  json <- lapply(lines, jsonlite::fromJSON, simplifyVector = FALSE)

  ids <- as.numeric(gsub("chat-", "", map_chr(json, "[[", "custom_id")))
  results <- lapply(json, "[[", "response")
  results[order(ids)]
}


method(batch_result_ok, ProviderOpenAI) <- function(provider, result) {
  result$status_code == 200
}

method(batch_result_turn, ProviderOpenAI) <- function(provider, result, has_type = FALSE) {
  value_turn(provider, result$body, has_type = has_type)
}
# OpenAI -> ellmer --------------------------------------------------------------

method(stream_parse, ProviderOpenAI) <- function(provider, event) {
  if (is.null(event)) {
    cli::cli_abort("Connection closed unexpectedly")
  }

  if (identical(event$data, "[DONE]")) {
    return(NULL)
  }

  jsonlite::parse_json(event$data)
}
method(stream_text, ProviderOpenAI) <- function(provider, event) {
  if (length(event$choices) == 0) {
    NULL
  } else {
    event$choices[[1]]$delta$content
  }

}
method(stream_merge_chunks, ProviderOpenAI) <- function(provider, result, chunk) {
  if (is.null(result)) {
    chunk
  } else {
    merge_dicts(result, chunk)
  }
}
method(value_turn, ProviderOpenAI) <- function(provider, result, has_type = FALSE) {
  if (has_name(result$choices[[1]], "delta")) { # streaming
    message <- result$choices[[1]]$delta
  } else {
    message <- result$choices[[1]]$message
  }

  if (has_type) {
    json <- jsonlite::parse_json(message$content[[1]])
    content <- list(ContentJson(json))
  } else {
    content <- lapply(message$content, as_content)
  }
  if (has_name(message, "tool_calls")) {
    calls <- lapply(message$tool_calls, function(call) {
      name <- call$`function`$name
      # TODO: record parsing error
      args <- jsonlite::parse_json(call$`function`$arguments)
      ContentToolRequest(name = name, arguments = args, id = call$id)
    })
    content <- c(content, calls)
  }
  tokens <- c(
    result$usage$prompt_tokens %||% NA_integer_,
    result$usage$completion_tokens %||% NA_integer_
  )
  tokens_log(paste0("OpenAI-", gsub("https?://", "", provider@base_url)), tokens)

  Turn(message$role, content, json = result, tokens = tokens)
}

# ellmer -> OpenAI --------------------------------------------------------------

method(as_json, list(ProviderOpenAI, Turn)) <- function(provider, x) {
  if (x@role == "system") {
    list(
      list(role = "system", content = x@contents[[1]]@text)
    )

  } else if (x@role == "user") {
    # Each tool result needs to go in its own message with role "tool"
    is_tool <- map_lgl(x@contents, S7_inherits, ContentToolResult)
    content <- as_json(provider, x@contents[!is_tool])
    if (length(content) > 0) {
      user <- list(list(role = "user", content = content))
    } else {
      user <- list()
    }

    tools <- lapply(x@contents[is_tool], function(tool) {
      list(role = "tool", content = tool_string(tool), tool_call_id = tool@id)
    })

    c(user, tools)
  } else if (x@role == "assistant") {
    # Tool requests come out of content and go into own argument
    is_tool <- map_lgl(x@contents, S7_inherits, ContentToolRequest)
    content <- as_json(provider, x@contents[!is_tool])
    tool_calls <- as_json(provider, x@contents[is_tool])

    list(
      compact(list(role = "assistant", content = content, tool_calls = tool_calls))
    )
  } else {
    cli::cli_abort("Unknown role {x@role}", .internal = TRUE)
  }
}

method(as_json, list(ProviderOpenAI, ContentText)) <- function(provider, x) {
  list(type = "text", text = x@text)
}

method(as_json, list(ProviderOpenAI, ContentImageRemote)) <- function(provider, x) {
  list(type = "image_url", image_url = list(url = x@url))
}

method(as_json, list(ProviderOpenAI, ContentImageInline)) <- function(provider, x) {
  list(
    type = "image_url",
    image_url = list(
      url = paste0("data:", x@type, ";base64,", x@data)
    )
  )
}

method(as_json, list(ProviderOpenAI, ContentToolRequest)) <- function(provider, x) {
  json_args <- jsonlite::toJSON(x@arguments)
  list(
    id = x@id,
    `function` = list(name = x@name, arguments = json_args),
    type = "function"
  )
}

method(as_json, list(ProviderOpenAI, ToolDef)) <- function(provider, x) {
  list(
    type = "function",
    "function" = compact(list(
      name = x@name,
      description = x@description,
      strict = TRUE,
      parameters = as_json(provider, x@arguments)
    ))
  )
}


method(as_json, list(ProviderOpenAI, TypeObject)) <- function(provider, x) {
  if (x@additional_properties) {
    cli::cli_abort("{.arg .additional_properties} not supported for OpenAI.")
  }

  names <- names2(x@properties)
  properties <- lapply(x@properties, function(x) {
    out <- as_json(provider, x)
    if (!x@required) {
      out$type <- c(out$type, "null")
    }
    out
  })

  names(properties) <- names

  list(
    type = "object",
    description = x@description %||% "",
    properties = properties,
    required = as.list(names),
    additionalProperties = FALSE
  )
}
