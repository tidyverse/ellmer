test_that("can make simple request", {
  vcr::local_cassette("anthropic-basic")

  chat <- chat_anthropic_test("Be as terse as possible; no punctuation")
  resp <- chat$chat("What is 1 + 1?")
  expect_match(resp, "2")
  expect_equal(unname(chat$last_turn()@tokens[1:2] > 0), c(TRUE, TRUE))
})

test_that("can make simple streaming request", {
  chat <- chat_anthropic_test(
    "Be as terse as possible; no punctuation"
  )
  resp <- coro::collect(chat$stream("What is 1 + 1?"))
  expect_match(paste0(unlist(resp), collapse = ""), "2")
})

test_that("can list models", {
  vcr::local_cassette("anthropic-list-models")

  test_models(models_anthropic)
})

# Common provider interface -----------------------------------------------

test_that("defaults are reported", {
  expect_snapshot(. <- chat_anthropic())
})

test_that("supports standard parameters", {
  vcr::local_cassette("anthropic-standard-params")
  test_params_stop(chat_anthropic_test)
})

test_that("supports tool calling", {
  vcr::local_cassette("anthropic-tool")
  chat_fun <- chat_anthropic_test

  test_tools_simple(chat_fun)
})

test_that("can fetch web pages", {
  vcr::local_cassette("anthropic-web-fetch")
  chat_fun <- \(...) {
    chat_anthropic_test(..., beta_headers = "web-fetch-2025-09-10")
  }
  test_tool_web_fetch(chat_fun, claude_tool_web_fetch())
})

test_that("can search web pages", {
  vcr::local_cassette("anthropic-web-search")
  chat_fun <- \(...) chat_anthropic_test(...)
  test_tool_web_search(chat_fun, claude_tool_web_search())
})

test_that("tools can return images", {
  vcr::local_cassette("anthropic-tool-image")
  chat_fun <- chat_anthropic_test
  test_tool_image(chat_fun)
})

test_that("can extract data", {
  vcr::local_cassette("anthropic-structured-data")
  chat_fun <- chat_anthropic_test

  test_data_extraction(chat_fun)
})

test_that("has_claude_structured_output() matches Claude 4.5+ and 5+ models", {
  expect_all_true(has_claude_structured_output(c(
    "claude-sonnet-4-5",
    "claude-opus-4-6",
    "claude-sonnet-5",
    "claude-sonnet-5-20260201",
    "claude-sonnet-6",
    "claude-opus-7-1",
    "claude-haiku-10"
  )))
  expect_all_equal(
    has_claude_structured_output(c(
      "claude-sonnet-4-0",
      "claude-3-5-sonnet-20241022",
      "claude-3-7-sonnet-20250219",
      "claude-4-sonnet-20250514"
    )),
    FALSE
  )
})

test_that("can use images", {
  vcr::local_cassette("anthropic-images")
  chat_fun <- chat_anthropic_test

  test_images_inline(chat_fun)
  test_images_remote(chat_fun)
})

test_that("can use pdfs", {
  vcr::local_cassette("anthropic-pdfs")
  chat_fun <- chat_anthropic_test

  test_pdf_local(chat_fun)
})

# Custom features --------------------------------------------------------

test_that("can set beta headers", {
  chat <- chat_anthropic_test(beta_headers = c("a", "b"))
  req <- chat_request(chat$get_provider(), chat$get_model_object())
  headers <- req_get_headers(req)
  expect_equal(headers$`anthropic-beta`, "a,b")
})

test_that("continues to work after whitespace only outputs (#376)", {
  vcr::local_cassette("anthropic-whitespace")

  chat <- chat_anthropic_test()
  chat$chat("Respond to this question with only two blank lines")
  expect_equal(
    chat$chat("What's 1+1? Just give me the number"),
    ellmer_output("2")
  )
})

test_that("can match prices for some common models", {
  chat <- chat_anthropic_test()

  expect_true(has_cost(chat$get_provider()@name, chat$get_model_object()@name))
})

test_that("removes empty final chat messages", {
  chat <- chat_anthropic_test()
  chat$set_turns(
    list(
      UserTurn("Don't say anything"),
      AssistantTurn()
    )
  )

  turns_json <- as_json(chat$get_provider(), chat$get_turns())

  expect_length(turns_json, 1)
  expect_equal(turns_json[[1]]$role, "user")
  expect_equal(
    turns_json[[1]]$content,
    list(list(type = "text", text = "Don't say anything"))
  )
})

test_that("batch chat works", {
  chat <- chat_anthropic_test(
    system_prompt = "Answer with just the city name. No formatting."
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
    path = test_path("batch/state-capitals-anthropic.json")
  )
  expect_equal(out, c("Des Moines", "Albany", "Sacramento", "Austin"))
})

test_that("value_turn() parses server_tool_use input from JSON string", {
  provider <- ProviderAnthropic(
    name = "Anthropic",
    base_url = "https://api.anthropic.com/v1",
    extra_headers = character(),
    credentials = NULL,
    beta_headers = character(),
    cache = ""
  )

  result <- list(
    content = list(
      list(
        type = "server_tool_use",
        id = "srvtoolu_1",
        name = "web_search",
        input = '{"query":"test search"}'
      )
    ),
    stop_reason = "end_turn",
    usage = list(
      input_tokens = 10,
      output_tokens = 5,
      cache_creation_input_tokens = 0,
      cache_read_input_tokens = 0
    )
  )

  turn <- value_turn(provider, test_model("claude-sonnet-4-20250514"), result)
  search_content <- turn@contents[[1]]
  expect_s7_class(search_content, ContentToolRequestSearch)
  expect_equal(search_content@query, "test search")
})

test_that("value_turn() parses server_tool_use web_fetch input from JSON string", {
  provider <- ProviderAnthropic(
    name = "Anthropic",
    base_url = "https://api.anthropic.com/v1",
    extra_headers = character(),
    credentials = NULL,
    beta_headers = character(),
    cache = ""
  )

  result <- list(
    content = list(
      list(
        type = "server_tool_use",
        id = "srvtoolu_2",
        name = "web_fetch",
        input = '{"url":"https://example.com"}'
      )
    ),
    stop_reason = "end_turn",
    usage = list(
      input_tokens = 10,
      output_tokens = 5,
      cache_creation_input_tokens = 0,
      cache_read_input_tokens = 0
    )
  )

  turn <- value_turn(provider, test_model("claude-sonnet-4-20250514"), result)
  fetch_content <- turn@contents[[1]]
  expect_s7_class(fetch_content, ContentToolRequestFetch)
  expect_equal(fetch_content@url, "https://example.com")
})

test_that("value_turn() prices cache writes at 1.25x while reporting raw tokens", {
  provider <- ProviderAnthropic(
    name = "Anthropic",
    base_url = "https://api.anthropic.com/v1",
    extra_headers = character(),
    credentials = NULL,
    beta_headers = character(),
    cache = ""
  )

  result <- list(
    content = list(list(type = "text", text = "ok")),
    stop_reason = "end_turn",
    usage = list(
      input_tokens = 1000,
      output_tokens = 50,
      cache_creation_input_tokens = 400,
      cache_read_input_tokens = 200
    )
  )

  turn <- value_turn(provider, test_model("claude-sonnet-4-20250514"), result)

  # tokens slot reports raw integer counts (no 1.25x weighting on input).
  expect_equal(
    unname(turn@tokens),
    c(1000 + 400, 50, 200)
  )

  # Cost matches the 1.25x cache-write weighting:
  #   (1000 + 400 * 1.25) * $3/1M + 50 * $15/1M + 200 * $0.30/1M
  expected_cost <- ((1000 + 400 * 1.25) * 3 + 50 * 15 + 200 * 0.30) / 1e6
  expect_equal(unclass(turn@cost), expected_cost)
})

test_that("value_turn() prices a refusal fallback at the serving model's rate", {
  provider <- ProviderAnthropic(
    name = "Anthropic",
    base_url = "https://api.anthropic.com/v1",
    extra_headers = character(),
    credentials = NULL,
    beta_headers = character(),
    cache = ""
  )
  model <- Model(name = "claude-fable-5")

  result <- list(
    model = "claude-opus-4-8",
    content = list(
      list(
        type = "fallback",
        from = list(model = "claude-fable-5"),
        to = list(model = "claude-opus-4-8")
      ),
      list(type = "text", text = "ok")
    ),
    stop_reason = "end_turn",
    usage = list(input_tokens = 1000, output_tokens = 50)
  )

  turn <- value_turn(provider, model, result)

  # opus-4-8 rates ($5/$25 per 1M), not fable-5's ($10/$50).
  expect_equal(unclass(turn@cost), (1000 * 5 + 50 * 25) / 1e6)
})

test_that("serving_model() prefers the last fallback block's to.model", {
  expect_equal(serving_model(list(model = "a", content = list())), "a")
  expect_equal(
    serving_model(list(
      model = "requested",
      content = list(
        list(type = "fallback", to = list(model = "served")),
        list(type = "text", text = "hi")
      )
    )),
    "served"
  )
})

test_that("stream_merge_chunks() handles citations_delta", {
  provider <- ProviderAnthropic(
    name = "Anthropic",
    base_url = "https://api.anthropic.com/v1",
    extra_headers = character(),
    credentials = NULL,
    beta_headers = character(),
    cache = ""
  )

  chunks <- list(
    list(
      type = "message_start",
      message = list(
        id = "msg_1",
        type = "message",
        role = "assistant",
        content = list(),
        model = "claude-sonnet-4-20250514",
        stop_reason = NULL,
        stop_sequence = NULL,
        usage = list(input_tokens = 10, output_tokens = 1)
      )
    ),
    list(
      type = "content_block_start",
      index = 0L,
      content_block = list(
        type = "text",
        text = ""
      )
    ),
    list(
      type = "content_block_delta",
      index = 0L,
      delta = list(
        type = "text_delta",
        text = "Hello"
      )
    ),
    list(
      type = "content_block_delta",
      index = 0L,
      delta = list(
        type = "citations_delta",
        citation = list(
          type = "web_search_result_location",
          cited_text = "example text",
          url = "https://example.com"
        )
      )
    ),
    list(type = "content_block_stop", index = 0L),
    list(
      type = "message_delta",
      delta = list(
        stop_reason = "end_turn",
        stop_sequence = NULL
      ),
      usage = list(output_tokens = 5)
    )
  )

  result <- NULL
  expect_no_warning({
    for (chunk in chunks) {
      result <- stream_merge_chunks(provider, result, chunk)
    }
  })

  expect_equal(result$content[[1]]$text, "Hello")
  expect_length(result$content[[1]]$citations, 1)
  expect_equal(result$content[[1]]$citations[[1]]$url, "https://example.com")
})

test_that("stream_content() emits Anthropic citations after completed text", {
  provider <- chat_anthropic_test()$get_provider()
  chunks <- list(
    list(
      type = "message_start",
      message = list(content = list(), usage = list())
    ),
    list(
      type = "content_block_start",
      index = 0L,
      content_block = list(type = "text", text = "", citations = list())
    ),
    list(
      type = "content_block_delta",
      index = 0L,
      delta = list(type = "text_delta", text = "Grounded answer")
    ),
    list(
      type = "content_block_delta",
      index = 0L,
      delta = list(
        type = "citations_delta",
        citation = list(
          type = "web_search_result_location",
          cited_text = "source evidence",
          url = "https://example.com",
          title = "Example"
        )
      )
    ),
    list(type = "content_block_stop", index = 0L)
  )

  completion <- NULL
  streamed <- list()
  for (chunk in chunks) {
    completion <- stream_merge_chunks(provider, completion, chunk)
    streamed <- c(streamed, stream_content(provider, chunk, completion))
  }

  expect_s7_class(streamed[[1]], ContentText)
  expect_equal(streamed[[1]]@text, "Grounded answer")
  expect_s7_class(streamed[[2]], ContentCitation)
  expect_equal(streamed[[2]]@grounded_span, "Grounded answer")
  expect_equal(streamed[[2]]@cited_quote, "source evidence")
})

test_that("value_turn() preserves Anthropic web activity and citations", {
  provider <- chat_anthropic_test()$get_provider()
  citation <- list(
    type = "web_search_result_location",
    cited_text = "source evidence",
    url = "https://example.com",
    title = "Example"
  )
  search_result <- list(
    type = "web_search_result",
    url = "https://search.example",
    title = "Search result"
  )
  result <- list(
    content = list(
      list(type = "text", text = "Grounded answer", citations = list(citation)),
      list(
        type = "web_search_tool_result",
        tool_use_id = "search-1",
        content = list(search_result)
      ),
      list(
        type = "web_fetch_tool_result",
        tool_use_id = "fetch-1",
        content = list(
          type = "web_fetch_result",
          url = "https://fetch.example"
        )
      ),
      list(
        type = "web_fetch_tool_result",
        tool_use_id = "fetch-2",
        content = list(
          type = "web_fetch_tool_result_error",
          error_code = "denied"
        )
      )
    ),
    stop_reason = "end_turn",
    usage = list(input_tokens = 10, output_tokens = 5)
  )

  contents <- value_turn(provider, test_model(), result)@contents
  expect_s7_class(contents[[1]], ContentText)
  expect_s7_class(contents[[2]], ContentCitation)
  expect_equal(contents[[2]]@source@url, "https://example.com")
  expect_equal(contents[[2]]@grounded_span, "Grounded answer")
  expect_equal(contents[[2]]@cited_quote, "source evidence")

  expect_s7_class(contents[[3]], ContentToolResponseSearch)
  expect_equal(contents[[3]]@sources[[1]]@title, "Search result")
  expect_equal(contents[[4]]@status, "success")
  expect_equal(contents[[4]]@url, "https://fetch.example")
  expect_equal(contents[[5]]@status, "error")
  expect_null(contents[[5]]@url)
})

test_that("value_turn() links Anthropic web-fetch citations to fetched URLs", {
  provider <- chat_anthropic_test()$get_provider()
  citation <- list(
    type = "char_location",
    cited_text = "second source evidence",
    document_index = 1L,
    document_title = "Second document",
    start_char_index = 0L,
    end_char_index = 22L
  )
  result <- list(
    content = list(
      list(
        type = "web_fetch_tool_result",
        tool_use_id = "fetch-1",
        content = list(
          type = "web_fetch_result",
          url = "https://first.example"
        )
      ),
      list(
        type = "web_fetch_tool_result",
        tool_use_id = "fetch-2",
        content = list(
          type = "web_fetch_result",
          url = "https://second.example"
        )
      ),
      list(
        type = "text",
        text = "Grounded answer",
        citations = list(citation)
      )
    ),
    stop_reason = "end_turn",
    usage = list(input_tokens = 10, output_tokens = 5)
  )

  contents <- value_turn(provider, test_model(), result)@contents
  expect_s7_class(contents[[4]], ContentCitation)
  expect_equal(contents[[4]]@source@url, "https://second.example")
  expect_equal(contents[[4]]@source@title, "Second document")
})

test_that("value_turn() resolves Anthropic citations across request turns", {
  provider <- chat_anthropic_test()$get_provider()
  prior_fetch <- list(
    type = "web_fetch_tool_result",
    tool_use_id = "fetch-prior",
    content = list(
      type = "web_fetch_result",
      url = "https://prior.example"
    )
  )
  turns <- list(
    UserTurn("Read the first page."),
    AssistantTurn(list(
      ContentToolResponseFetch(
        url = "https://prior.example",
        status = "success",
        extra = prior_fetch
      )
    )),
    UserTurn("Compare it with the second page.")
  )
  result <- list(
    content = list(
      list(
        type = "web_fetch_tool_result",
        tool_use_id = "fetch-current",
        content = list(
          type = "web_fetch_result",
          url = "https://current.example"
        )
      ),
      list(
        type = "text",
        text = "Comparison",
        citations = list(
          list(
            type = "char_location",
            cited_text = "prior evidence",
            document_index = 0L,
            document_title = "Prior document",
            start_char_index = 0L,
            end_char_index = 14L
          ),
          list(
            type = "char_location",
            cited_text = "current evidence",
            document_index = 1L,
            document_title = "Current document",
            start_char_index = 15L,
            end_char_index = 31L
          )
        )
      )
    ),
    stop_reason = "end_turn",
    usage = list(input_tokens = 10, output_tokens = 5)
  )

  contents <- value_turn_with_turns(
    provider,
    test_model(),
    result,
    turns = turns
  )@contents
  citations <- keep(contents, \(x) S7_inherits(x, ContentCitation))

  expect_equal(citations[[1]]@source@url, "https://prior.example")
  expect_equal(citations[[2]]@source@url, "https://current.example")
})

test_that("value_turn() preserves unresolved Anthropic document slots", {
  provider <- chat_anthropic_test()$get_provider()
  turns <- list(UserTurn(list(
    ContentPDF("application/pdf", "ZGF0YQ==", "report.pdf"),
    ContentUploaded("file-pdf", "application/pdf"),
    ContentUploaded("file-csv", "text/csv"),
    ContentText("Compare the documents with the fetched page.")
  )))
  result <- list(
    content = list(
      list(
        type = "web_fetch_tool_result",
        tool_use_id = "fetch-current",
        content = list(
          type = "web_fetch_result",
          url = "https://current.example"
        )
      ),
      list(
        type = "text",
        text = "Comparison",
        citations = list(
          list(
            type = "char_location",
            cited_text = "inline PDF evidence",
            document_index = 0L,
            document_title = "report.pdf",
            start_char_index = 0L,
            end_char_index = 19L
          ),
          list(
            type = "char_location",
            cited_text = "uploaded PDF evidence",
            document_index = 1L,
            document_title = "uploaded.pdf",
            start_char_index = 20L,
            end_char_index = 41L
          ),
          list(
            type = "char_location",
            cited_text = "web evidence",
            document_index = 2L,
            document_title = "Current document",
            start_char_index = 42L,
            end_char_index = 54L
          )
        )
      )
    ),
    stop_reason = "end_turn",
    usage = list(input_tokens = 10, output_tokens = 5)
  )

  contents <- value_turn_with_turns(
    provider,
    test_model(),
    result,
    turns = turns
  )@contents
  citations <- keep(contents, \(x) S7_inherits(x, ContentCitation))

  expect_null(citations[[1]]@source)
  expect_null(citations[[2]]@source)
  expect_equal(citations[[3]]@source@url, "https://current.example")
})

test_that("stream_content() resolves Anthropic citations across request turns", {
  provider <- chat_anthropic_test()$get_provider()
  prior_fetch <- list(
    type = "web_fetch_tool_result",
    tool_use_id = "fetch-prior",
    content = list(
      type = "web_fetch_result",
      url = "https://prior.example"
    )
  )
  turns <- list(AssistantTurn(list(
    ContentToolResponseFetch(
      url = "https://prior.example",
      status = "success",
      extra = prior_fetch
    )
  )))
  completion <- list(
    content = list(list(
      type = "text",
      text = "Grounded answer",
      citations = list(list(
        type = "char_location",
        cited_text = "prior evidence",
        document_index = 0L,
        document_title = "Prior document",
        start_char_index = 0L,
        end_char_index = 14L
      ))
    ))
  )

  contents <- stream_content_with_turns(
    provider,
    list(type = "content_block_stop", index = 0L),
    completion,
    turns = turns
  )

  expect_equal(contents[[1]]@source@url, "https://prior.example")
})

# Token counting -----------------------------------------------------------

test_that("can count tokens", {
  vcr::local_cassette("anthropic-count-tokens")
  test_token_count(chat_anthropic_test)
})
