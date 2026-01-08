#' @include provider.R
#' @include content.R
#' @include turns.R
#' @include tools-def.R
NULL

#' Chat with a Google Gemini or Vertex AI model
#'
#' @description
#' Google's AI offering is broken up into two parts: Gemini and Vertex AI.
#' Most enterprises are likely to use Vertex AI, and individuals are likely
#' to use Gemini.
#'
#' Use [google_upload()] to upload files (PDFs, images, video, audio, etc.)
#'
#' ## Authentication
#' These functions try a number of authentication strategies, in this order:
#'
#' * An API key set in the `GOOGLE_API_KEY` env var, or,
#'   for `chat_google_gemini()` only, `GEMINI_API_KEY`.
#' * Google's default application credentials, if the \pkg{gargle} package
#'   is installed.
#' * Viewer-based credentials on Posit Connect, if the \pkg{connectcreds}
#'   package.
#' * `r lifecycle::badge("experimental")`. An browser-based OAuth flow, if
#'   you're in an interactive session. This currently uses an unverified
#'   OAuth app (so you will get a scary warning); we plan to verify in the
#'   near future.
#'
#' @param api_key `r lifecycle::badge("deprecated")` Use `credentials` instead.
#' @param credentials A function that returns a list of authentication headers
#'   or `NULL`, the default, to use ambient credentials. See above for details.
#' @param model `r param_model("gemini-2.5-flash", "google_gemini")`
#' @inheritParams chat_openai
#' @inherit chat_openai return
#' @family chatbots
#' @export
#' @examples
#' \dontrun{
#' chat <- chat_google_gemini()
#' chat$chat("Tell me three jokes about statisticians")
#' }
chat_google_gemini <- function(
  system_prompt = NULL,
  base_url = "https://generativelanguage.googleapis.com/v1beta/",
  api_key = NULL,
  credentials = NULL,
  model = NULL,
  params = NULL,
  api_args = list(),
  api_headers = character(),
  echo = NULL
) {
  model <- set_default(model, "gemini-2.5-flash")
  echo <- check_echo(echo)

  credentials <- as_credentials(
    "chat_google_gemini",
    default_google_credentials(variant = "gemini"),
    credentials = credentials,
    api_key = api_key
  )

  provider <- ProviderGoogleGemini(
    name = "Google/Gemini",
    base_url = base_url,
    model = model,
    params = params %||% params(),
    extra_args = api_args,
    extra_headers = api_headers,
    credentials = credentials
  )
  Chat$new(provider = provider, system_prompt = system_prompt, echo = echo)
}

chat_google_gemini_test <- function(
  ...,
  model = "gemini-2.5-flash",
  params = NULL,
  echo = "none"
) {
  params <- params %||% params()
  params$temperature <- params$temperature %||% 0
  params$seed <- 1014

  chat_google_gemini(..., model = model, params = params, echo = echo)
}

#' @export
#' @rdname chat_google_gemini
#' @param location Location, e.g. `us-east1`, `me-central1`, `africa-south1` or
#'   `global`.
#' @param project_id Project ID.
chat_google_vertex <- function(
  location,
  project_id,
  system_prompt = NULL,
  model = NULL,
  params = NULL,
  api_args = list(),
  api_headers = character(),
  echo = NULL
) {
  check_string(location)
  check_string(project_id)

  model <- set_default(model, "gemini-2.5-flash")
  echo <- check_echo(echo)
  credentials <- default_google_credentials(variant = "vertex")

  provider <- ProviderGoogleGemini(
    name = "Google/Vertex",
    base_url = vertex_url(location, project_id),
    model = model,
    params = params %||% params(),
    extra_args = api_args,
    extra_headers = api_headers,
    credentials = credentials
  )
  Chat$new(provider = provider, system_prompt = system_prompt, echo = echo)
}

# https://cloud.google.com/vertex-ai/docs/reference/rest/v1/projects.locations.endpoints/generateContent
vertex_url <- function(location, project_id) {
  paste_c(
    c("https://", google_location(location), "aiplatform.googleapis.com"),
    "/v1",
    c("/projects/", project_id),
    c("/locations/", location),
    "/publishers/google/"
  )
}

ProviderGoogleGemini <- new_class(
  "ProviderGoogleGemini",
  parent = Provider,
  properties = list(
    model = prop_string()
  )
)

# Base request -----------------------------------------------------------------

method(base_request, ProviderGoogleGemini) <- function(provider) {
  req <- request(provider@base_url)
  req <- ellmer_req_credentials(req, provider@credentials(), "x-goog-api-key")
  req <- ellmer_req_robustify(req)
  req <- ellmer_req_user_agent(req)
  req <- req_error(req, body = function(resp) {
    json <- resp_body_json(resp, check_type = FALSE)
    json$error$message
  })
  req
}

# Chat -------------------------------------------------------------------------

method(chat_request, ProviderGoogleGemini) <- function(
  provider,
  stream = TRUE,
  turns = list(),
  tools = list(),
  type = NULL
) {
  req <- base_request(provider)

  # Can't use chat_path() because it varies based on stream
  req <- req_url_path_append(req, "models")
  if (stream) {
    # https://ai.google.dev/api/generate-content#method:-models.streamgeneratecontent
    req <- req_url_path_append(
      req,
      paste0(provider@model, ":", "streamGenerateContent")
    )
    req <- req_url_query(req, alt = "sse")
  } else {
    # https://ai.google.dev/api/generate-content#method:-models.generatecontent
    req <- req_url_path_append(
      req,
      paste0(provider@model, ":", "generateContent")
    )
  }

  body <- chat_body(
    provider = provider,
    stream = stream,
    turns = turns,
    tools = tools,
    type = type
  )
  body <- modify_list(body, provider@extra_args)

  req <- req_body_json(req, body)
  req <- req_headers(req, !!!provider@extra_headers)
  req
}

method(chat_body, ProviderGoogleGemini) <- function(
  provider,
  stream = TRUE,
  turns = list(),
  tools = list(),
  type = NULL
) {
  if (length(turns) >= 1 && is_system_turn(turns[[1]])) {
    system <- list(parts = list(text = turns[[1]]@text))
  } else {
    system <- list(parts = list(text = ""))
  }

  generation_config <- chat_params(provider, provider@params)
  if (!is.null(type)) {
    generation_config$response_mime_type <- "application/json"
    generation_config$response_schema <- as_json(provider, type)
  }

  if (has_name(generation_config, "thinkingBudget")) {
    generation_config$thinkingConfig <- list(
      thinkingBudget = generation_config$thinkingBudget,
      includeThoughts = TRUE
    )
    generation_config$thinkingBudget <- NULL
  }

  contents <- as_json(provider, turns)

  # https://ai.google.dev/api/caching#Tool
  if (length(tools) > 0) {
    is_builtin <- map_lgl(tools, \(tool) S7_inherits(tool, ToolBuiltIn))
    funs <- as_json(provider, unname(tools))

    tools <- c(
      compact(list(functionDeclarations = funs[!is_builtin])),
      unlist(funs[is_builtin], recursive = FALSE)
    )
  } else {
    tools <- NULL
  }

  compact(list(
    contents = contents,
    tools = tools,
    systemInstruction = system,
    generationConfig = generation_config
  ))
}

method(chat_params, ProviderGoogleGemini) <- function(provider, params) {
  standardise_params(
    params,
    c(
      temperature = "temperature",
      topP = "top_p",
      topK = "top_k",
      frequencyPenalty = "frequency_penalty",
      presencePenalty = "presence_penalty",
      seed = "seed",
      maxOutputTokens = "max_tokens",
      responseLogprobs = "log_probs",
      stopSequences = "stop_sequences",
      thinkingBudget = "reasoning_tokens"
    )
  )
}

# Gemini -> ellmer --------------------------------------------------------------

method(stream_parse, ProviderGoogleGemini) <- function(provider, event) {
  if (is.null(event)) {
    NULL
  } else {
    jsonlite::parse_json(event$data)
  }
}
method(stream_content, ProviderGoogleGemini) <- function(provider, event) {
  parts <- event$candidates[[1]]$content$parts
  if (is.null(parts) || length(parts) == 0) {
    return(NULL)
  }

  part <- parts[[1]]
  if (isTRUE(part$thought) && !is.null(part$text)) {
    ContentThinking(part$text)
  } else if (!is.null(part$text)) {
    ContentText(part$text)
  }
}
method(stream_merge_chunks, ProviderGoogleGemini) <- function(
  provider,
  result,
  chunk
) {
  if (is.null(result)) {
    chunk
  } else {
    merge_gemini_chunks(result, chunk)
  }
}

method(value_tokens, ProviderGoogleGemini) <- function(provider, json) {
  # https://ai.google.dev/api/generate-content#UsageMetadata
  usage <- json$usageMetadata

  # Total token count for the generation request (prompt + response candidates).
  # Not documented, but appears to include thinking and tool use, i.e.
  # usage$promptTokenCount + usage$candidatesTokenCount +
  #  usage$toolUsePromptTokenCount + usage$thoughtsTokenCount ==
  #  usage$totalTokenCount
  total <- usage$totalTokenCount %||% 0

  # Number of tokens in the prompt. When cachedContent is set, this is
  # still the total effective prompt size meaning this includes the number
  # of tokens in the cached content.
  input <- usage$promptTokenCount %||% 0

  cached <- usage$cachedContentTokenCount %||% 0

  tokens(
    input = input - cached,
    output = total - input,
    cached_input = cached
  )
}

method(value_turn, ProviderGoogleGemini) <- function(
  provider,
  result,
  has_type = FALSE
) {
  message <- result$candidates[[1]]$content

  contents <- lapply(message$parts, function(content) {
    if (isTRUE(content$thought) && has_name(content, "text")) {
      ContentThinking(content$text)
    } else if (has_name(content, "text")) {
      if (has_type) {
        ContentJson(string = content$text)
      } else {
        ContentText(content$text)
      }
    } else if (has_name(content, "functionCall")) {
      extra <- compact(list(
        thoughtSignature = content$thoughtSignature
      ))
      ContentToolRequest(
        content$functionCall$name,
        content$functionCall$name,
        content$functionCall$args,
        extra = extra
      )
    } else if (has_name(content, "inlineData")) {
      ContentImageInline(
        type = content$inlineData$mimeType,
        data = content$inlineData$data
      )
    } else {
      cli::cli_abort(
        "Unknown content type with names {.str {names(content)}}.",
        .internal = TRUE
      )
    }
  })
  contents <- compact(contents)
  tokens <- value_tokens(provider, result)
  cost <- get_token_cost(provider, tokens)
  AssistantTurn(contents, json = result, tokens = unlist(tokens), cost = cost)
}

# ellmer -> Gemini --------------------------------------------------------------

# https://ai.google.dev/api/caching#Content
method(as_json, list(ProviderGoogleGemini, Turn)) <- function(
  provider,
  x,
  ...
) {
  if (is_system_turn(x)) {
    # System messages go in the top-level API parameter
  } else if (is_user_turn(x)) {
    x <- turn_contents_expand(x)
    list(
      role = x@role,
      parts = as_json(provider, x@contents, ...)
    )
  } else if (is_assistant_turn(x)) {
    list(role = "model", parts = as_json(provider, x@contents, ...))
  } else {
    cli::cli_abort("Unknown role {x@role}", .internal = TRUE)
  }
}


method(as_json, list(ProviderGoogleGemini, ToolDef)) <- function(
  provider,
  x,
  ...
) {
  compact(list(
    name = x@name,
    description = x@description,
    parameters = as_json(provider, x@arguments, ...)
  ))
}

method(as_json, list(ProviderGoogleGemini, ContentText)) <- function(
  provider,
  x,
  ...
) {
  if (identical(x@text, "")) {
    # Gemini tool call requests can include a Content with empty text,
    # but it doesn't like it if you send this back
    NULL
  } else {
    list(text = x@text)
  }
}

method(as_json, list(ProviderGoogleGemini, ContentThinking)) <- function(
  provider,
  x,
  ...
) {
  # https://ai.google.dev/gemini-api/docs/thinking
  list(thought = TRUE, text = x@thinking)
}

method(as_json, list(ProviderGoogleGemini, ContentPDF)) <- function(
  provider,
  x,
  ...
) {
  list(
    inlineData = list(
      mimeType = x@type,
      data = x@data
    )
  )
}

# https://ai.google.dev/api/caching#FileData
method(as_json, list(ProviderGoogleGemini, ContentUploaded)) <- function(
  provider,
  x,
  ...
) {
  list(
    fileData = list(
      mimeType = x@mime_type,
      fileUri = x@uri
    )
  )
}

# https://ai.google.dev/api/caching#FileData
method(as_json, list(ProviderGoogleGemini, ContentImageRemote)) <- function(
  provider,
  x,
  ...
) {
  cli::cli_abort("Gemini doesn't support remote images")
}

# https://ai.google.dev/api/caching#Blob
method(as_json, list(ProviderGoogleGemini, ContentImageInline)) <- function(
  provider,
  x,
  ...
) {
  list(
    inlineData = list(
      mimeType = x@type,
      data = x@data
    )
  )
}

# https://ai.google.dev/api/caching#FunctionCall
method(as_json, list(ProviderGoogleGemini, ContentToolRequest)) <- function(
  provider,
  x,
  ...
) {
  compact(list(
    functionCall = list(
      name = x@id,
      args = x@arguments
    ),
    thoughtSignature = x@extra$thoughtSignature
  ))
}

# https://ai.google.dev/api/caching#FunctionResponse
method(as_json, list(ProviderGoogleGemini, ContentToolResult)) <- function(
  provider,
  x,
  ...
) {
  list(
    functionResponse = list(
      name = x@request@id,
      response = list(value = tool_string(x))
    )
  )
}

method(as_json, list(ProviderGoogleGemini, TypeObject)) <- function(
  provider,
  x,
  ...
) {
  if (x@additional_properties) {
    cli::cli_abort("{.arg .additional_properties} not supported for Gemini.")
  }

  if (length(x@properties) == 0) {
    return(list())
  }

  required <- map_lgl(x@properties, function(prop) prop@required)

  compact(list(
    type = "object",
    description = x@description,
    properties = as_json(provider, x@properties, ...),
    required = as.list(names2(x@properties)[required])
  ))
}

# Gemini-specific merge logic --------------------------------------------------

merge_last <- function() {
  function(left, right, path = NULL) {
    right
  }
}

merge_identical <- function() {
  function(left, right, path = NULL) {
    if (!identical(left, right)) {
      stop(
        "Expected identical values, but got ",
        deparse(left),
        " and ",
        deparse(right)
      )
    }
    left
  }
}

merge_any_or_empty <- function() {
  function(left, right, path = NULL) {
    if (!is.null(left) && nzchar(left)) {
      left
    } else if (!is.null(right) && nzchar(right)) {
      right
    } else {
      ""
    }
  }
}

merge_optional <- function(merge_func) {
  function(left, right, path = NULL) {
    if (is.null(left) && is.null(right)) {
      NULL
    } else {
      merge_func(left, right, path)
    }
  }
}

merge_objects <- function(...) {
  spec <- list(...)
  function(left, right, path = NULL) {
    if (is.null(left)) {
      return(right)
    } else if (is.null(right)) {
      return(left)
    }

    # cat(paste(collapse = "", path), "\n")
    stopifnot(is.list(left), is.list(right), all(nzchar(names(spec))))
    mapply(
      names(spec),
      spec,
      FUN = function(key, value) {
        value(left[[key]], right[[key]], c(path, ".", key))
      },
      USE.NAMES = TRUE,
      SIMPLIFY = FALSE
    )
  }
}

merge_candidate_lists <- function(...) {
  merge_unindexed <- merge_objects(...)
  merge_indexed <- merge_objects(index = merge_identical(), ...)

  function(left, right, path = NULL) {
    if (length(left) == 1 && length(right) == 1) {
      list(merge_unindexed(left[[1]], right[[1]], c(path, "[]")))
    } else {
      # left and right are lists of objects with [["index"]]
      # We need to find the elements that have matching indices and merge them
      left_indices <- vapply(left, `[[`, integer(1), "index")
      right_indices <- vapply(right, `[[`, integer(1), "index")
      # I know this seems weird, but according to Google's Go SDK, we should
      # only retain indices on the right that *already* appear on the left.
      # Citations:
      # https://github.com/google/generative-ai-go/blob/3d14f4039eaef321b15bcbf70839389d7f000233/genai/client_test.go#L655
      # https://github.com/google/generative-ai-go/blob/3d14f4039eaef321b15bcbf70839389d7f000233/genai/client.go#L396
      lapply(left_indices, function(index) {
        left_item <- left[[which(left_indices == index)]]
        right_item <- right[[which(right_indices == index)]]
        if (is.null(right_item)) {
          left_item
        } else {
          merge_indexed(left_item, right_item, c(path, "[", index, "]"))
        }
      })
    }
  }
}

merge_append <- function() {
  function(left, right, path = NULL) {
    c(left, right)
  }
}

merge_parts <- function() {
  function(left, right, path = NULL) {
    joined <- c(left, right)

    # Identify text parts
    is_text <- map_lgl(joined, ~ is.list(.x) && identical(names(.x), "text"))

    # Create groups for contiguous sections
    groups <- cumsum(c(TRUE, diff(is_text) != 0))

    # Split into groups and process each
    split_parts <- split(joined, groups)
    merged_split_parts <- map2(
      split_parts,
      split(is_text, groups),
      function(parts, is_text_group) {
        if (!is_text_group[[1]]) {
          # Non-text group: return parts unchanged
          return(parts)
        } else {
          # Text group: merge text values
          text_values <- map_chr(parts, ~ .x[["text"]])
          list(list(text = paste0(text_values, collapse = "")))
        }
      }
    )
    unlist(merged_split_parts, recursive = FALSE, use.names = FALSE)
  }
}

# Put it all together...
# https://ai.google.dev/api/generate-content#v1beta.GenerateContentResponse
merge_gemini_chunks <- merge_objects(
  candidates = merge_candidate_lists(
    content = merge_objects(
      role = merge_any_or_empty(),
      parts = merge_parts()
    ),
    finishReason = merge_last(),
    safetyRatings = merge_last(),
    citationMetadata = merge_optional(
      merge_objects(citationSources = merge_append())
    ),
    tokenCount = merge_last()
  ),
  promptFeedback = merge_last(),
  usageMetadata = merge_last()
)

default_google_credentials <- function(
  error_call = caller_env(),
  variant = c("gemini", "vertex")
) {
  variant <- arg_match(variant)

  api_key <- Sys.getenv("GOOGLE_API_KEY")
  if (variant == "gemini" && api_key == "") {
    api_key <- Sys.getenv("GEMINI_API_KEY")
  }
  if (nzchar(api_key)) {
    return(\() api_key)
  }

  gemini_scope <- switch(
    variant,
    gemini = "https://www.googleapis.com/auth/generative-language.retriever",
    # https://github.com/googleapis/python-genai/blob/cc9e470326e0c1b84ec3ce9891c9f96f6c74688e/google/genai/_api_client.py#L184
    vertex = "https://www.googleapis.com/auth/cloud-platform"
  )

  # Detect viewer-based credentials from Posit Connect.
  if (has_connect_viewer_token(scope = gemini_scope)) {
    return(function() {
      token <- connectcreds::connect_viewer_token(scope = gemini_scope)
      list(Authorization = paste("Bearer", token$access_token))
    })
  }

  if (is_testing()) {
    testthat::skip_if_not_installed("gargle")
  }

  check_installed("gargle", "for Google authentication")
  gargle::with_cred_funs(
    funs = list(
      # We don't want to use *all* of gargle's default credential functions --
      # in particular, we don't want to try and authenticate using the bundled
      # OAuth client -- so winnow down the list.
      credentials_app_default = gargle::credentials_app_default
    ),
    {
      token <- gargle::token_fetch(scopes = gemini_scope)
    },
    action = "replace"
  )

  if (is.null(token) && is_testing()) {
    testthat::skip("no Google credentials available")
  }

  if (is_interactive()) {
    return(function() {
      function(req) {
        req_oauth_auth_code(
          req,
          client = gemini_client(),
          auth_url = "https://accounts.google.com/o/oauth2/auth",
          scope = "https://www.googleapis.com/auth/generative-language.retriever"
        )
      }
    })
  }

  if (is.null(token)) {
    cli::cli_abort(
      c(
        "No Google credentials are available.",
        "i" = "Try suppling an API key or configuring Google's application default credentials."
      ),
      call = error_call
    )
  }

  # gargle emits an httr-style token, which we awkwardly shim into something
  # httr2 can work with.

  if (!token$can_refresh()) {
    # TODO: Not really sure what to do in this case when the token expires.
    return(function() {
      list(Authorization = paste("Bearer", token$credentials$access_token))
    })
  }

  # gargle tokens don't track the expiry time, so we do it ourselves (with a
  # grace period).
  expiry <- Sys.time() + token$credentials$expires_in - 5
  return(function() {
    if (expiry < Sys.time()) {
      token$refresh()
    }
    list(Authorization = paste("Bearer", token$credentials$access_token))
  })
}

google_oauth_reset <- function() {
  httr2::oauth_cache_clear(gemini_client())
}

# Pricing ----------------------------------------------------------------------

# Models -----------------------------------------------------------------------

#' @export
#' @rdname chat_google_gemini
models_google_gemini <- function(
  base_url = "https://generativelanguage.googleapis.com/v1beta/",
  api_key = NULL,
  credentials = NULL
) {
  check_string(base_url)

  credentials <- as_credentials(
    "models_google_gemini",
    default_google_credentials(variant = "gemini"),
    credentials = credentials,
    api_key = api_key
  )

  models_google(base_url, credentials = credentials, variant = "gemini")
}

#' @rdname chat_google_gemini
#' @export
models_google_vertex <- function(location, project_id, credentials = NULL) {
  check_string(location)
  check_string(project_id)

  credentials <- credentials %||% default_google_credentials(variant = "vertex")
  check_credentials(credentials)

  base_url <- paste_c(
    c("https://", google_location(location), "aiplatform.googleapis.com"),
    "/v1beta1",
    "/publishers/google/"
  )

  models_google(base_url, project_id = project_id, variant = "vertex")
}

models_google <- function(
  base_url = "https://generativelanguage.googleapis.com/v1beta/",
  credentials,
  project_id = NULL,
  variant = c("gemini", "vertex")
) {
  variant <- arg_match(variant)

  provider <- ProviderGoogleGemini(
    name = "Google/Gemini",
    model = "",
    base_url = base_url,
    # https://cloud.google.com/docs/authentication/troubleshoot-adc#user-creds-client-based
    credentials = credentials
  )

  req <- base_request(provider)
  if (variant == "vertex") {
    req <- req_headers(req, `x-goog-user-project` = project_id)
  }
  req <- req_headers(req, !!!provider@extra_headers)
  req <- req_url_path_append(req, "/models")
  resp <- req_perform(req)

  json <- resp_body_json(resp)

  if (variant == "vertex") {
    name <- map_chr(json$publisherModels, "[[", "name")
    name <- gsub("^publishers/google/models/", "", name)
    # this is the closest to "generateContent" in "supportedGenerationMethods" for Gemini
    # https://cloud.google.com/vertex-ai/docs/reference/rest/v1beta1/publishers.models
    can_generate <- json$publisherModels |>
      map_lgl(\(x) "openGenerationAiStudio" %in% names(x$supportedActions))
  } else {
    name <- map_chr(json$models, "[[", "name")
    name <- gsub("^models/", "", name)
    display_name <- map_chr(json$models, "[[", "displayName")

    methods <- map(json$models, \(x) unlist(x$supportedGenerationMethods))
    can_generate <- map_lgl(methods, \(x) "generateContent" %in% x)
  }

  df <- data.frame(id = name)
  df <- cbind(df, match_prices(provider@name, df$id))
  df <- df[can_generate, ]
  unrowname(df[order(df$id), ])
}

# for location "global", there is no location in the final base URL
# https://github.com/googleapis/python-genai/blob/cc9e470326e0c1b84ec3ce9891c9f96f6c74688e/google/genai/_api_client.py#L646-L654
google_location <- function(location) {
  if (location == "global") "" else paste0(location, "-")
}
