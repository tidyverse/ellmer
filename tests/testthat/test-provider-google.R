# Getting started --------------------------------------------------------

test_that("can make simple request", {
  chat <- chat_google_gemini_test("Be as terse as possible; no punctuation")
  resp <- chat$chat("What is 1 + 1?")
  expect_match(resp, "2")
  expect_equal(unname(chat$last_turn()@tokens[1:2] > 0), c(TRUE, TRUE))
})

test_that("can make simple streaming request", {
  chat <- chat_google_gemini_test("Be as terse as possible; no punctuation")
  resp <- coro::collect(chat$stream("What is 1 + 1?"))
  expect_match(paste0(unlist(resp), collapse = ""), "2")
})

test_that("can handle errors", {
  chat <- chat_google_gemini_test(model = "doesnt-exist")
  expect_snapshot(chat$chat("Hi"), error = TRUE)
})

test_that("can list models", {
  test_models(models_google_gemini)
})

# Common provider interface -----------------------------------------------

test_that("defaults are reported", {
  expect_snapshot(. <- chat_google_gemini())
})

test_that("supports standard parameters", {
  chat_fun <- chat_google_gemini_test

  test_params_stop(chat_fun)
})

test_that("supports tool calling", {
  vcr::local_cassette("google-tool")
  chat_fun <- chat_google_gemini_test

  test_tools_simple(chat_fun)
})

test_that("can extract data", {
  chat_fun <- chat_google_gemini_test

  test_data_extraction(chat_fun)
})

test_that("can fetch web pages", {
  vcr::local_cassette("google-web-fetch")
  chat_fun <- chat_google_gemini_test
  test_tool_web_fetch(chat_fun, google_tool_web_fetch())
})

test_that("can search web pages", {
  vcr::local_cassette("google-web-search")
  chat_fun <- chat_google_gemini_test
  test_tool_web_search(chat_fun, google_tool_web_search())
})

test_that("can combine built-in and user tools", {
  chat <- chat_google_gemini_test()
  provider <- chat$get_provider()
  model <- chat$get_model_object()

  regular_tool <- tool(
    function(x) x,
    "Return `x`",
    arguments = list(x = type_number("x"))
  )

  body <- chat_body(
    provider,
    model,
    stream = TRUE,
    turns = list(Turn("user", "hi")),
    tools = list(regular_tool, google_tool_web_search())
  )

  expect_length(body$tools, 2)
  expect_named(body$tools[[1]], "functionDeclarations")
  expect_named(body$tools[[2]], "google_search")
})

test_that("can use images", {
  vcr::local_cassette("google-image")
  chat_fun <- chat_google_gemini_test

  test_images_inline(chat_fun)
  test_images_remote_error(chat_fun)
})

test_that("can use pdfs", {
  chat_fun <- chat_google_gemini_test

  test_pdf_local(chat_fun)
})

test_that("file lifecycle works", {
  vcr::local_cassette("google-files")
  test_file_lifecycle(chat_google_gemini_test)
})

test_that("can use documents", {
  chat_fun <- chat_google_gemini_test

  test_document_local(chat_fun)
})

test_that("binary documents are rejected", {
  provider <- chat_google_gemini_test()$get_provider()

  xls <- ContentDocument("application/vnd.ms-excel", "YQ==", "old.xls")
  expect_snapshot(as_json(provider, xls), error = TRUE)
})

test_that("can match prices for some common models", {
  provider <- chat_google_gemini_test()$get_provider()

  expect_true(has_cost(provider@name, "gemini-3.7-flash"))

  expect_false(has_cost(provider@name, "gemini-1.0-pro-latest"))
})

# custom behaviour -------------------------------------------------------------

test_that("vertex generates expected base_url", {
  chat <- chat_google_vertex("{location}", "{project}")

  service_endpoint <- "https://{location}-aiplatform.googleapis.com/v1"
  model <- "/projects/{project}/locations/{location}/publishers/google/"
  expect_equal(chat$get_provider()@base_url, paste0(service_endpoint, model))
})

test_that("can merge text output", {
  # output from "tell me a joke" with text changed
  messages <- c(
    '{"candidates": [{"content": {"parts": [{"text": "a"}],"role": "model"}}],"usageMetadata": {"promptTokenCount": 5,"totalTokenCount": 5},"modelVersion": "gemini-1.5-flash"}',
    '{"candidates": [{"content": {"parts": [{"text": "b"}],"role": "model"}}],"usageMetadata": {"promptTokenCount": 5,"totalTokenCount": 5},"modelVersion": "gemini-1.5-flash"}',
    '{"candidates": [{"content": {"parts": [{"text": "c"}],"role": "model"},"finishReason": "STOP"}],"usageMetadata": {"promptTokenCount": 5,"candidatesTokenCount": 17,"totalTokenCount": 22},"modelVersion": "gemini-1.5-flash"}'
  )
  chunks <- lapply(messages, jsonlite::parse_json)

  out <- merge_gemini_chunks(chunks[[1]], chunks[[2]])
  out <- merge_gemini_chunks(out, chunks[[3]])

  expect_equal(out$candidates[[1]]$content$parts[[1]]$text, "abc")
  expect_equal(
    out$usageMetadata,
    list(
      promptTokenCount = 5,
      candidatesTokenCount = 17,
      totalTokenCount = 22
    )
  )
  expect_equal(out$candidates[[1]]$finishReason, "STOP")
})

test_that("merge_gemini_chunks() retains final grounding metadata", {
  text <- list(
    candidates = list(
      list(
        content = list(
          parts = list(list(text = "Grounded answer")),
          role = "model"
        )
      )
    )
  )
  metadata <- list(
    candidates = list(
      list(
        groundingMetadata = list(
          webSearchQueries = list("ellmer citations")
        ),
        urlContextMetadata = list(
          urlMetadata = list(
            list(
              retrievedUrl = "https://example.com",
              urlRetrievalStatus = "URL_RETRIEVAL_STATUS_SUCCESS"
            )
          )
        )
      )
    )
  )

  merged <- merge_gemini_chunks(text, metadata)
  candidate <- merged$candidates[[1]]
  expect_equal(
    candidate$groundingMetadata$webSearchQueries,
    list("ellmer citations")
  )
  expect_equal(
    candidate$urlContextMetadata$urlMetadata[[1]]$retrievedUrl,
    "https://example.com"
  )

  # Metadata is also retained when a later chunk lacks it
  merged <- merge_gemini_chunks(metadata, text)
  expect_equal(
    merged$candidates[[1]]$groundingMetadata$webSearchQueries,
    list("ellmer citations")
  )
  expect_equal(
    merged$candidates[[1]]$urlContextMetadata$urlMetadata[[1]]$retrievedUrl,
    "https://example.com"
  )
})

test_that("can handle citations", {
  # based on "Write me a 5-paragraph essay on the history of the tidyverse."
  messages <- c(
    '{"candidates": [{"content": {"parts": [{"text": "a"}]}, "role": "model"}]}',
    '{"candidates": [{
      "content": {"parts": [{"text": "a"}]},
      "role": "model",
      "citationMetadata": {
        "citationSources": [
          {
            "startIndex": 1,
            "endIndex": 2,
            "uri": "https://example.com",
            "license": ""
          }
        ]
      }
    }]}'
  )
  chunks <- lapply(messages, jsonlite::parse_json)

  out <- merge_gemini_chunks(chunks[[1]], chunks[[2]])
  source <- out$candidates[[1]]$citationMetadata$citationSources[[1]]
  expect_equal(source$startIndex, 1)
  expect_equal(source$endIndex, 2)
  expect_equal(source$uri, "https://example.com")
  expect_equal(source$license, "")
})

test_that("value_turn() preserves Google web metadata", {
  provider <- chat_google_gemini_test()$get_provider()
  support_with_source <- list(
    segment = list(text = "Grounded answer"),
    groundingChunkIndices = list(0L)
  )
  support_without_source <- list(
    segment = list(text = "Source-less answer"),
    groundingChunkIndices = list(1L)
  )
  grounding <- list(
    webSearchQueries = list("ellmer citations"),
    groundingChunks = list(
      list(web = list(uri = "https://example.com", title = "Example")),
      list(retrievedContext = list(title = "No web URL"))
    ),
    groundingSupports = list(support_with_source, support_without_source)
  )
  url_metadata <- list(
    retrievedUrl = "https://fetch.example",
    urlRetrievalStatus = "URL_RETRIEVAL_STATUS_SUCCESS"
  )
  result <- list(
    candidates = list(
      list(
        content = list(
          role = "model",
          parts = list(list(text = "Grounded answer and source-less answer"))
        ),
        finishReason = "STOP",
        groundingMetadata = grounding,
        urlContextMetadata = list(urlMetadata = list(url_metadata))
      )
    ),
    usageMetadata = list()
  )

  contents <- value_turn(provider, test_model(), result)@contents
  expect_s7_class(contents[[1]], ContentToolRequestSearch)
  expect_equal(contents[[1]]@query, "ellmer citations")
  expect_s7_class(contents[[2]], ContentToolResponseSearch)
  expect_equal(contents[[2]]@sources[[1]]@title, "Example")
  expect_s7_class(contents[[3]], ContentText)
  expect_s7_class(contents[[4]], ContentCitation)
  expect_equal(contents[[4]]@grounded_span, "Grounded answer")
  expect_equal(contents[[4]]@source@url, "https://example.com")
  expect_s7_class(contents[[5]], ContentCitation)
  expect_equal(contents[[5]]@grounded_span, "Source-less answer")
  expect_null(contents[[5]]@source)
  expect_s7_class(contents[[6]], ContentToolRequestFetch)
  expect_s7_class(contents[[7]], ContentToolResponseFetch)
  expect_equal(contents[[7]]@status, "success")
})

test_that("stream_content() emits Google citations before activity on the final chunk", {
  provider <- chat_google_gemini_test()$get_provider()
  grounding <- list(
    webSearchQueries = list("ellmer citations"),
    groundingChunks = list(
      list(web = list(uri = "https://example.com", title = "Example"))
    ),
    groundingSupports = list(
      list(
        segment = list(text = "Grounded answer"),
        groundingChunkIndices = list(0L)
      )
    )
  )
  event <- list(
    candidates = list(
      list(
        content = list(parts = list(list(text = "Grounded answer"))),
        groundingMetadata = grounding,
        finishReason = "STOP"
      )
    )
  )

  streamed <- stream_content(provider, event, completion = event)
  expect_s7_class(streamed[[1]], ContentText)
  expect_s7_class(streamed[[2]], ContentCitation)
  expect_s7_class(streamed[[3]], ContentToolRequestSearch)
  expect_s7_class(streamed[[4]], ContentToolResponseSearch)
  expect_equal(streamed[[2]]@grounded_span, "Grounded answer")

  metadata_only <- list(
    candidates = list(
      list(groundingMetadata = grounding, finishReason = "STOP")
    )
  )
  streamed <- stream_content(provider, metadata_only, completion = event)
  expect_length(streamed, 3)
  expect_s7_class(streamed[[1]], ContentCitation)
})

test_that("stream_content() defers early Google fetch activity until citations", {
  provider <- chat_google_gemini_test()$get_provider()
  chunks <- list(
    list(
      candidates = list(
        list(
          content = list(parts = list(list(text = "A grounded answer was "))),
          urlContextMetadata = list(
            urlMetadata = list(
              list(
                retrievedUrl = "https://example.com",
                urlRetrievalStatus = "URL_RETRIEVAL_STATUS_SUCCESS"
              )
            )
          )
        )
      )
    ),
    list(
      candidates = list(
        list(
          content = list(parts = list(list(text = "released today."))),
          groundingMetadata = list(
            webSearchQueries = list("ellmer citations"),
            groundingChunks = list(
              list(web = list(uri = "https://example.com", title = "Example"))
            ),
            groundingSupports = list(
              list(
                segment = list(text = "released today."),
                groundingChunkIndices = list(0L)
              )
            )
          ),
          finishReason = "STOP"
        )
      )
    )
  )

  completion <- NULL
  streamed <- list()
  for (chunk in chunks) {
    completion <- stream_merge_chunks(provider, completion, chunk)
    streamed <- c(streamed, stream_content(provider, chunk, completion))
  }

  expect_length(streamed, 7)
  expect_s7_class(streamed[[1]], ContentText)
  expect_s7_class(streamed[[2]], ContentText)
  expect_s7_class(streamed[[3]], ContentCitation)
  expect_s7_class(streamed[[4]], ContentToolRequestSearch)
  expect_s7_class(streamed[[5]], ContentToolResponseSearch)
  expect_s7_class(streamed[[6]], ContentToolRequestFetch)
  expect_s7_class(streamed[[7]], ContentToolResponseFetch)
})

test_that("can generate images", {
  vcr::local_cassette("google-image-gen")

  chat <- chat_google_gemini_test(model = "gemini-2.5-flash-image")
  chat$chat("Draw a cat")

  turn <- chat$get_turns()[[2]]
  expect_s7_class(turn@contents[[1]], ContentImageInline)
})

test_that("can use thinking levels", {
  vcr::local_cassette("google-thinking-level")

  chat <- chat_google_gemini_test(
    model = "gemini-3.7-flash",
    params = params(temperature = 0, reasoning_effort = "low")
  )
  resp <- chat$chat("What is 1 + 1?", echo = FALSE)

  contents <- chat$last_turn()@contents
  thinking <- Filter(\(x) S7::S7_inherits(x, ContentThinking), contents)
  expect_length(thinking, 1)
  expect_gt(nchar(thinking[[1]]@thinking), 0)
  expect_match(resp, "2")
})

test_that("batch chat works", {
  chat <- chat_google_gemini_test(
    system_prompt = "Answer with just the city name"
  )

  prompts <- list(
    "What's the capital of Iowa?",
    "What's the capital of New York?",
    "What's the capital of California?",
    "What's the capital of Texas?"
  )

  out <- batch_chat_text(
    chat,
    prompts,
    path = test_path("batch/state-capitals-gemini.json")
  )
  expect_equal(out, c("Des Moines", "Albany", "Sacramento", "Austin"))
})

test_that("gemini_prepare_batch_body handles API quirks", {
  chat <- chat_google_gemini_test()
  provider <- chat$get_provider()
  model <- chat$get_model_object()

  body <- chat_body(
    provider,
    model,
    stream = FALSE,
    turns = list(Turn("user", "hi")),
    type = type_object(firstName = type_string())
  )
  result <- gemini_prepare_batch_body(body)

  # Batch JSONL parser requires snake_case (HTTP 400 with camelCase)
  expect_true("generation_config" %in% names(result))
  expect_null(result$generationConfig)

  # Batch JSONL parser uses response_json_schema, not response_schema;
  # schema property names like "firstName" must survive snake_case conversion
  expect_true(
    "firstName" %in%
      names(result$generation_config$response_json_schema$properties)
  )
  expect_null(result$generation_config$response_schema)

  # Batch JSONL parser rejects empty system instruction text
  body$systemInstruction <- list(parts = list(text = ""))
  expect_null(gemini_prepare_batch_body(body)$system_instruction)

  body$systemInstruction <- list(parts = list(text = "Be helpful."))
  expect_equal(
    gemini_prepare_batch_body(body)$system_instruction$parts$text,
    "Be helpful."
  )
})

test_that("batch_status waits for responsesFile after SUCCEEDED", {
  provider <- chat_google_gemini_test()$get_provider()

  returned_batch <- list(
    metadata = list(
      state = "BATCH_STATE_SUCCEEDED",
      batchStats = list(requestCount = 2L, successfulRequestCount = 2L)
    )
  )

  no_file <- batch_status(provider, returned_batch)
  expect_true(no_file$working)

  returned_batch$response <- list(responsesFile = "files/abc123")

  with_file <- batch_status(
    provider,
    returned_batch
  )
  expect_false(with_file$working)
})

# Token counting -----------------------------------------------------------

test_that("can count tokens", {
  vcr::local_cassette("google-count-tokens")
  test_token_count(chat_google_gemini_test)
})
