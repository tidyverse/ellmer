test_that("can make simple batch request", {
  vcr::local_cassette("anthropic-batch")

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
  req <- chat_request(chat$get_provider())
  headers <- req_get_headers(req)
  expect_equal(headers$`anthropic-beta`, "a,b")
})

test_that("continues to work after whitespace only outputs (#376)", {
  vcr::local_cassette("anthropic-whitespace")

  chat <- chat_anthropic_test()
  chat$chat("Respond with only two blank lines")
  expect_equal(
    chat$chat("What's 1+1? Just give me the number"),
    ellmer_output("2")
  )
})

test_that("can match prices for some common models", {
  provider <- chat_anthropic_test()$get_provider()

  expect_true(has_cost(provider, "claude-sonnet-4-20250514"))
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
  chat <- chat_anthropic_test(system_prompt = "Answer with just the city name")

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
    model = "claude-sonnet-4-20250514",
    params = list(),
    extra_args = list(),
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

  turn <- value_turn(provider, result)
  search_content <- turn@contents[[1]]
  expect_s7_class(search_content, ContentToolRequestSearch)
  expect_equal(search_content@query, "test search")
})

test_that("value_turn() parses server_tool_use web_fetch input from JSON string", {
  provider <- ProviderAnthropic(
    name = "Anthropic",
    base_url = "https://api.anthropic.com/v1",
    model = "claude-sonnet-4-20250514",
    params = list(),
    extra_args = list(),
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

  turn <- value_turn(provider, result)
  fetch_content <- turn@contents[[1]]
  expect_s7_class(fetch_content, ContentToolRequestFetch)
  expect_equal(fetch_content@url, "https://example.com")
})

test_that("value_turn() prices cache writes at 1.25x while reporting raw tokens", {
  provider <- ProviderAnthropic(
    name = "Anthropic",
    base_url = "https://api.anthropic.com/v1",
    model = "claude-sonnet-4-20250514",
    params = list(),
    extra_args = list(),
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

  turn <- value_turn(provider, result)

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

test_that("stream_merge_chunks() handles citations_delta", {
  provider <- ProviderAnthropic(
    name = "Anthropic",
    base_url = "https://api.anthropic.com/v1",
    model = "claude-sonnet-4-20250514",
    params = list(),
    extra_args = list(),
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

# Thinking stream behavior -------------------------------------------------

# Anthropic emits a single reasoning block before the response text, so we
# expect exactly one start...body...end run with all thinking chunks
# preceding all text chunks. Providers that interleave reasoning and output
# (e.g. OpenAI Responses) would need a different expectation.
expect_thinking_delta_phases <- function(chunks) {
  thinking_chunks <- Filter(
    \(x) S7::S7_inherits(x, ContentThinkingDelta),
    chunks
  )
  text_chunks <- Filter(\(x) S7::S7_inherits(x, ContentText), chunks)

  expect_gt(length(thinking_chunks), 0)
  expect_gt(length(text_chunks), 0)

  expect_equal(thinking_chunks[[1]]@phase, "start")
  expect_equal(thinking_chunks[[length(thinking_chunks)]]@phase, "end")
  expect_equal(thinking_chunks[[length(thinking_chunks)]]@thinking, "")
  if (length(thinking_chunks) > 2) {
    middle_phases <- vapply(
      thinking_chunks[2:(length(thinking_chunks) - 1)],
      \(x) x@phase,
      character(1)
    )
    expect_all_true(middle_phases == "body")
  }

  is_thinking <- vapply(
    chunks,
    \(x) S7::S7_inherits(x, ContentThinkingDelta),
    logical(1)
  )
  is_text <- vapply(chunks, \(x) S7::S7_inherits(x, ContentText), logical(1))
  expect_lt(max(which(is_thinking)), which(is_text)[1])
}

test_that("stream='text' suppresses thinking", {
  chat <- chat_anthropic_test(
    params = params(budget_tokens = 2048, temperature = 1)
  )
  chunks <- coro::collect(chat$stream("What is 2+2?", stream = "text"))
  text <- paste0(chunks, collapse = "")

  expect_no_match(text, "<thinking>")
  expect_no_match(text, "</thinking>")
  expect_match(text, "4")
})

test_that("stream='content' yields ContentThinkingDelta with phases", {
  chat <- chat_anthropic_test(
    params = params(budget_tokens = 2048, temperature = 1)
  )

  chunks <- coro::collect(chat$stream("What is 2+2?", stream = "content"))
  expect_thinking_delta_phases(chunks)

  # Final turn stores ContentThinking (not deltas)
  turn_contents <- chat$last_turn()@contents
  thinking <- Filter(\(x) S7::S7_inherits(x, ContentThinking), turn_contents)
  expect_length(thinking, 1)
  expect_gt(nchar(thinking[[1]]@thinking), 0)
})

test_that("stream_async='content' yields ContentThinkingDelta with phases", {
  chat <- chat_anthropic_test(
    params = params(budget_tokens = 2048, temperature = 1)
  )

  chunks <- sync(coro::async_collect(
    chat$stream_async("What is 2+2?", stream = "content")
  ))
  expect_thinking_delta_phases(chunks)

  turn_contents <- chat$last_turn()@contents
  thinking <- Filter(\(x) S7::S7_inherits(x, ContentThinking), turn_contents)
  expect_length(thinking, 1)
  expect_gt(nchar(thinking[[1]]@thinking), 0)
})

test_that("partial turn from interrupted thinking stream can be replayed", {
  chat <- chat_anthropic_test(
    params = params(budget_tokens = 2048, temperature = 1)
  )

  # Break out as soon as the first thinking chunk arrives; the partial turn
  # is left in chat$.turns and must serialize cleanly on follow-up.
  saw_thinking <- FALSE
  coro::loop(
    for (chunk in chat$stream("What is 2+2?", stream = "content")) {
      if (S7::S7_inherits(chunk, ContentThinkingDelta)) {
        saw_thinking <- TRUE
        break
      }
    }
  )
  expect_true(saw_thinking)

  partial <- chat$last_turn()
  delta_count <- sum(vapply(
    partial@contents,
    \(x) S7::S7_inherits(x, ContentThinkingDelta),
    logical(1)
  ))
  expect_equal(delta_count, 0)

  # Follow-up call must not error on as_json dispatch over the partial turn.
  expect_no_error(chat$chat("What about 3+3?", echo = "none"))
})
