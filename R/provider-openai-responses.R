#' @include provider.R
#' @include content.R
#' @include turns.R
#' @include tools-def.R
NULL

#' Chat with an OpenAI model using the responses API
#'
#' @description
#' The responses API is the latest interface to [OpenAI](https://openai.com/)'s
#' models. You will need to use it if you want to access the built-in tools
#' like image generation and web search.
#'
#' Note that a ChatGPT Plus membership does not grant access to the API.
#' You will need to sign up for a developer account (and pay for it) at the
#' [developer platform](https://platform.openai.com).
#'
#' @inheritParams chat_openai
#' @family chatbots
#' @export
#' @returns A [Chat] object.
#' @examples
#' \dontshow{ellmer:::vcr_example_start("chat_openai_responses")}
#' chat <- chat_openai_responses()
#' chat$chat("
#'   What is the difference between a tibble and a data frame?
#'   Answer with a bulleted list
#' ")
#'
#' chat$chat("Tell me three funny jokes about statisticians")
#' \dontshow{ellmer:::vcr_example_end()}
chat_openai_responses <- function(
  system_prompt = NULL,
  base_url = Sys.getenv("OPENAI_BASE_URL", "https://api.openai.com/v1"),
  api_key = openai_key(),
  model = NULL,
  params = NULL,
  api_args = list(),
  api_headers = character(),
  echo = c("none", "output", "all")
) {
  model <- set_default(model, "gpt-4.1")
  echo <- check_echo(echo)

  provider <- ProviderOpenAIResponses(
    name = "OpenAI",
    base_url = base_url,
    model = model,
    params = params %||% params(),
    extra_args = api_args,
    extra_headers = api_headers,
    api_key = api_key
  )
  Chat$new(provider = provider, system_prompt = system_prompt, echo = echo)
}
chat_openai_responses_test <- function(
  system_prompt = "Be terse.",
  ...,
  model = "gpt-4.1-nano",
  params = NULL,
  echo = "none"
) {
  params <- params %||% params()
  params$temperature <- params$temperature %||% 0

  chat_openai_responses(
    system_prompt = system_prompt,
    model = model,
    params = params,
    ...,
    echo = echo
  )
}

ProviderOpenAIResponses <- new_class(
  "ProviderOpenAIResponses",
  parent = ProviderOpenAI,
  properties = list(
    prop_redacted("api_key"),
    # no longer used by OpenAI itself; but subclasses still need it
    seed = prop_number_whole(allow_null = TRUE)
  )
)

# Chat endpoint ----------------------------------------------------------------

method(chat_path, ProviderOpenAIResponses) <- function(provider) {
  "/responses"
}

# https://platform.openai.com/docs/api-reference/responses
method(chat_body, ProviderOpenAIResponses) <- function(
  provider,
  stream = TRUE,
  turns = list(),
  tools = list(),
  type = NULL
) {
  input <- compact(unlist(as_json(provider, turns), recursive = FALSE))
  tools <- as_json(provider, unname(tools))

  if (!is.null(type)) {
    # https://platform.openai.com/docs/api-reference/responses/create#responses-create-text
    text <- list(
      format = list(
        type = "json_schema",
        name = "structured_data",
        schema = as_json(provider, type),
        strict = TRUE
      )
    )
  } else {
    text <- NULL
  }

  # https://platform.openai.com/docs/api-reference/responses/create#responses-create-include
  params <- chat_params(provider, provider@params)

  include <- c(
    if (isTRUE(params$log_probs)) "message.output_text.logprobs",
    if (is_openai_reasoning(provider@model)) "reasoning.encrypted_content"
  )
  params$log_probs <- NULL

  compact(list2(
    input = input,
    include = as.list(include),
    model = provider@model,
    !!!params,
    stream = stream,
    tools = tools,
    text = text,
    store = FALSE
  ))
}


method(chat_params, ProviderOpenAIResponses) <- function(provider, params) {
  standardise_params(
    params,
    c(
      temperature = "temperature",
      top_p = "top_p",
      frequency_penalty = "frequency_penalty",
      max_tokens = "max_output_tokens",
      log_probs = "log_probs",
      top_logprobs = "top_k"
    )
  )
}

# OpenAI -> ellmer --------------------------------------------------------------

method(stream_text, ProviderOpenAIResponses) <- function(provider, event) {
  # https://platform.openai.com/docs/api-reference/responses-streaming/response/output_text/delta
  if (event$type == "response.output_text.delta") {
    event$delta
  }
}
method(stream_merge_chunks, ProviderOpenAIResponses) <- function(
  provider,
  result,
  chunk
) {
  # https://platform.openai.com/docs/api-reference/responses-streaming/response/completed
  if (chunk$type == "response.completed") {
    chunk$response
  }
}

method(value_tokens, ProviderOpenAIResponses) <- function(provider, json) {
  usage <- json$usage
  cached_tokens <- usage$input_tokens_details$cached_tokens %||% 0

  tokens(
    input = (usage$input_tokens %||% 0) - cached_tokens,
    output = usage$output_tokens,
    cached_input = cached_tokens
  )
}

method(value_turn, ProviderOpenAIResponses) <- function(
  provider,
  result,
  has_type = FALSE
) {
  contents <- lapply(result$output, function(output) {
    if (output$type == "message") {
      if (has_type) {
        ContentJson(jsonlite::parse_json(output$content[[1]]$text))
      } else {
        ContentText(output$content[[1]]$text)
      }
    } else if (output$type == "function_call") {
      arguments <- jsonlite::parse_json(output$arguments)
      ContentToolRequest(output$id, output$name, arguments)
    } else if (output$type == "reasoning") {
      # {
      #   id: str,
      #   summary: str,
      #   type: "reasoning",
      #   content: [
      #     { text: str, type: "reasoning_text" }
      #   ],
      #   encrypted_content: str,
      #   status: "in_progress" | "completed" | "incomplete"
      # }
      thinking <- paste0(map_chr(output$content, "[[", "text"), collapse = "")
      ContentThinking(thinking = thinking, extra = output)
    } else if (output$type == "image_generation_call") {
      mime_type <- switch(
        output$output_format,
        png = "image/png",
        jpeg = "image/jpeg",
        webp = "image/webp",
        "unknown"
      )
      ContentImageInline(mime_type, output$result)
    } else {
      cli::cli_abort(
        "Unknown content type {.str {content$type}}.",
        .internal = TRUE
      )
    }
  })

  tokens <- value_tokens(provider, result)
  tokens_log(provider, tokens)
  assistant_turn(contents = contents, json = result, tokens = unlist(tokens))
}

# ellmer -> OpenAI --------------------------------------------------------------

method(as_json, list(ProviderOpenAIResponses, Turn)) <- function(
  provider,
  x,
  ...
) {
  # While the user turn can contain multiple contents, the assistant turn
  # can't. Fortunately, we can send multiple user turns with out issue.
  as_json(provider, x@contents, ..., role = x@role)
}

method(as_json, list(ProviderOpenAIResponses, ContentText)) <- function(
  provider,
  x,
  ...,
  role
) {
  type <- if (role %in% c("user", "system")) "input_text" else "output_text"
  list(
    role = role,
    content = list(list(type = type, text = x@text))
  )
}

method(as_json, list(ProviderOpenAIResponses, ContentThinking)) <- function(
  provider,
  x,
  ...
) {
  x@extra
}

method(as_json, list(ProviderOpenAIResponses, ContentImageRemote)) <- function(
  provider,
  x,
  ...
) {
  list(
    role = "user",
    content = list(
      list(type = "input_image", image_url = x@url)
    )
  )
}

method(as_json, list(ProviderOpenAIResponses, ContentImageInline)) <- function(
  provider,
  x,
  ...
) {
  list(
    role = "user",
    content = list(
      list(
        type = "input_image",
        image_url = paste0("data:", x@type, ";base64,", x@data)
      )
    )
  )
}

method(as_json, list(ProviderOpenAIResponses, ContentPDF)) <- function(
  provider,
  x,
  ...
) {
  # https://platform.openai.com/docs/guides/pdf-files?api-mode=responses
  list(
    role = "user",
    content = list(list(
      type = "input_file",
      filename = x@filename,
      file_data = paste0("data:application/pdf;base64,", x@data)
    ))
  )
}

method(as_json, list(ProviderOpenAIResponses, ContentToolRequest)) <- function(
  provider,
  x,
  ...
) {
  list(
    type = "function_call",
    call_id = x@id,
    name = x@name,
    arguments = jsonlite::toJSON(x@arguments)
  )
}

method(as_json, list(ProviderOpenAIResponses, ContentToolResult)) <- function(
  provider,
  x,
  ...
) {
  list(
    type = "function_call_output",
    call_id = x@request@id,
    output = tool_string(x)
  )
}

method(as_json, list(ProviderOpenAIResponses, ToolDef)) <- function(
  provider,
  x,
  ...
) {
  list(
    type = "function",
    name = x@name,
    description = x@description,
    strict = TRUE,
    parameters = as_json(provider, x@arguments, ...)
  )
}

# Batched requests -------------------------------------------------------------

method(has_batch_support, ProviderOpenAIResponses) <- function(provider) {
  TRUE
}

# https://platform.openai.com/docs/api-reference/batch
method(batch_submit, ProviderOpenAIResponses) <- function(
  provider,
  conversations,
  type = NULL
) {
  path <- withr::local_tempfile()

  # First put the requests in a file
  # https://platform.openai.com/docs/api-reference/batch/request-input
  requests <- map(seq_along(conversations), function(i) {
    body <- chat_body(
      provider,
      stream = FALSE,
      turns = conversations[[i]],
      type = type
    )

    list(
      custom_id = paste0("chat-", i),
      method = "POST",
      url = "/v1/responses",
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
  req <- req_body_json(
    req,
    list(
      input_file_id = uploaded$id,
      endpoint = "/v1/chat/completions",
      completion_window = "24h"
    )
  )

  resp <- req_perform(req)
  resp_body_json(resp)
}

# Helpers ------------------------------------------------------------------

is_openai_reasoning <- function(model) {
  # https://platform.openai.com/docs/models/compare
  startsWith(model, "o") || startsWith(model, "gpt-5")
}
