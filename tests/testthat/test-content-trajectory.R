# Structural helper --------------------------------------------------------

expect_valid_trajectory_records <- function(records) {
  for (record in records) {
    expect_type(record$role, "character")
    expect_contains(
      c("meta", "user", "reasoning", "assistant", "tool"),
      record$role
    )

    if (record$role == "assistant") {
      has_tool_calls <- !is.null(record$tool_calls) &&
        length(record$tool_calls) > 0
      if (has_tool_calls) {
        expect_null(record$content)
        for (call in record$tool_calls) {
          expect_named(call, c("id", "name", "args"))
          expect_type(call$id, "character")
          expect_type(call$name, "character")
          expect_type(call$args, "character")
        }
      } else {
        expect_type(record$content, "character")
        expect_gt(nchar(record$content), 0)
      }
    } else if (record$role == "tool") {
      expect_type(record$tool_call_id, "character")
      expect_type(record$content, "character")
    } else if (record$role %in% c("user", "reasoning")) {
      expect_type(record$content, "character")
    }
  }
  invisible(records)
}

skip_if_no_jsonvalidate <- function() {
  testthat::skip_if_not_installed("jsonvalidate")
}

expect_valid_against_schema <- function(records) {
  skip_if_no_jsonvalidate()
  validator <- jsonvalidate::json_validator(
    schema = test_path("schema/trajectory-v1-relaxed.schema.json"),
    engine = "ajv"
  )
  # `null = "null"` renders `content = NULL` entries as JSON null (rather
  # than jsonlite's default `{}`), matching the schema's `["string", "null"]`.
  json <- jsonlite::toJSON(records, auto_unbox = TRUE, null = "null")
  expect_equal(validator(json, verbose = TRUE), TRUE)
}

# Records --------------------------------------------------------------------

test_that("plain text exchange produces user/assistant records", {
  records <- c(
    contents_trajectory(UserTurn("Hello there")),
    contents_trajectory(AssistantTurn("Hi!"))
  )

  expect_equal(records[[1]], list(role = "user", content = "Hello there"))
  expect_equal(records[[2]], list(role = "assistant", content = "Hi!"))
})

test_that("tool call and result round trip", {
  request <- ContentToolRequest("call_1", "my_tool", list(x = 1))
  assistant_records <- contents_trajectory(AssistantTurn(list(request)))
  user_records <- contents_trajectory(UserTurn(list(
    ContentToolResult(value = "42", request = request)
  )))

  expect_length(assistant_records, 1)
  expect_null(assistant_records[[1]]$content)
  expect_length(assistant_records[[1]]$tool_calls, 1)
  expect_equal(assistant_records[[1]]$tool_calls[[1]]$id, "call_1")
  expect_equal(assistant_records[[1]]$tool_calls[[1]]$name, "my_tool")
  expect_type(assistant_records[[1]]$tool_calls[[1]]$args, "character")

  expect_length(user_records, 1)
  expect_equal(user_records[[1]]$role, "tool")
  expect_equal(user_records[[1]]$tool_call_id, "call_1")
  expect_equal(user_records[[1]]$content, "42")

  expect_valid_trajectory_records(c(assistant_records, user_records))
  expect_valid_against_schema(c(assistant_records, user_records))
})

test_that("thinking content produces a reasoning record", {
  turn <- AssistantTurn(list(
    ContentText("Before"),
    ContentThinking("Let me think"),
    ContentText("After")
  ))

  records <- contents_trajectory(turn)

  expect_length(records, 3)
  expect_equal(records[[1]], list(role = "assistant", content = "Before"))
  expect_equal(records[[2]], list(role = "reasoning", content = "Let me think"))
  expect_equal(records[[3]], list(role = "assistant", content = "After"))
})

test_that("text/tool_calls/text splits into three assistant records", {
  req1 <- ContentToolRequest("call_1", "tool_one", list())
  req2 <- ContentToolRequest("call_2", "tool_two", list())

  turn <- AssistantTurn(list(
    ContentText("Let me check that."),
    req1,
    req2,
    ContentText("Here's what I found.")
  ))

  records <- contents_trajectory(turn)

  expect_length(records, 3)
  expect_equal(
    records[[1]],
    list(role = "assistant", content = "Let me check that.")
  )
  expect_null(records[[2]]$content)
  expect_length(records[[2]]$tool_calls, 2)
  expect_equal(records[[2]]$tool_calls[[1]]$id, "call_1")
  expect_equal(records[[2]]$tool_calls[[2]]$id, "call_2")
  expect_equal(
    records[[3]],
    list(role = "assistant", content = "Here's what I found.")
  )
})

test_that("built-in web search ids match between call and result", {
  turn <- AssistantTurn(list(
    ContentToolRequestSearch(query = "ellmer package", json = list()),
    ContentToolResponseSearch(
      urls = c("https://a.com", "https://b.com"),
      json = list()
    )
  ))

  records <- contents_trajectory(turn, turn_index = 2)

  expect_length(records, 2)
  call_id <- records[[1]]$tool_calls[[1]]$id
  expect_equal(call_id, "websearch_2_1")
  expect_equal(records[[1]]$tool_calls[[1]]$name, "web_search")
  expect_equal(records[[2]]$tool_call_id, call_id)
  expect_equal(records[[2]]$content, "https://a.com\nhttps://b.com")
})

test_that("multiple built-in searches link each result to its own request", {
  turn <- AssistantTurn(list(
    ContentToolRequestSearch(query = "first", json = list()),
    ContentToolRequestSearch(query = "second", json = list()),
    ContentToolResponseSearch(urls = "https://first.com", json = list()),
    ContentToolResponseSearch(urls = "https://second.com", json = list())
  ))

  records <- contents_trajectory(turn, turn_index = 2)

  expect_length(records, 3)
  tool_calls <- records[[1]]$tool_calls
  expect_equal(
    vapply(tool_calls, \(x) x$id, character(1)),
    c(
      "websearch_2_1",
      "websearch_2_2"
    )
  )
  expect_equal(records[[2]]$tool_call_id, "websearch_2_1")
  expect_equal(records[[2]]$content, "https://first.com")
  expect_equal(records[[3]]$tool_call_id, "websearch_2_2")
  expect_equal(records[[3]]$content, "https://second.com")
})

test_that("built-in result without a request falls back to a synthesized id", {
  turn <- AssistantTurn(list(
    ContentToolResponseSearch(urls = "https://a.com", json = list()),
    ContentToolResponseFetch(url = "https://b.com", json = list())
  ))

  records <- contents_trajectory(turn, turn_index = 4)

  expect_length(records, 2)
  expect_equal(records[[1]]$tool_call_id, "websearch_4_1")
  expect_equal(records[[2]]$tool_call_id, "webfetch_4_2")
})

test_that("built-in web fetch ids match between call and result", {
  turn <- AssistantTurn(list(
    ContentToolRequestFetch(url = "https://example.com", json = list()),
    ContentToolResponseFetch(url = "https://example.com", json = list())
  ))

  records <- contents_trajectory(turn, turn_index = 3)

  expect_length(records, 2)
  call_id <- records[[1]]$tool_calls[[1]]$id
  expect_equal(call_id, "webfetch_3_1")
  expect_equal(records[[1]]$tool_calls[[1]]$name, "web_fetch")
  expect_equal(records[[2]]$tool_call_id, call_id)
  expect_equal(records[[2]]$content, "https://example.com")
})

test_that("ContentJson renders as real assistant content", {
  turn_string <- AssistantTurn(list(
    ContentJson(data = NULL, string = '{"a":1}')
  ))
  turn_data <- AssistantTurn(list(
    ContentJson(data = list(a = 1), string = NULL)
  ))

  records_string <- contents_trajectory(turn_string)
  records_data <- contents_trajectory(turn_data)

  expect_type(records_string[[1]]$content, "character")
  expect_gt(nchar(records_string[[1]]$content), 0)
  expect_type(records_data[[1]]$content, "character")
  expect_gt(nchar(records_data[[1]]$content), 0)
})

test_that("image, pdf, and uploaded content degrade to placeholder text", {
  turn <- AssistantTurn(list(
    ContentText("Here's what I found:"),
    content_image_url("https://example.com/plot.png"),
    ContentPDF("application/pdf", "ZGF0YQ==", "report.pdf"),
    ContentUploaded("https://example.com/file123", "text/csv")
  ))

  records <- contents_trajectory(turn)

  expect_length(records, 1)
  content <- records[[1]]$content
  expect_match(content, "[image: https://example.com/plot.png]", fixed = TRUE)
  expect_match(content, "[pdf: report.pdf]", fixed = TRUE)
  expect_match(
    content,
    "[uploaded file: https://example.com/file123]",
    fixed = TRUE
  )
  expect_gt(nchar(content), 0)
})

test_that("system prompt is omitted from chat trajectories", {
  chat <- Chat$new(test_provider(model = "test-model"))
  chat$set_turns(list(
    SystemTurn("Be terse."),
    UserTurn("Hi"),
    AssistantTurn("Hello!")
  ))

  records <- contents_trajectory(chat)

  expect_equal(
    records[[1]],
    list(role = "meta", source = "ellmer", model = "test-model")
  )
  expect_length(records, 3)
  roles <- vapply(records, \(record) record$role, character(1))
  expect_equal(roles, c("meta", "user", "assistant"))

  texts <- c(records[[2]]$content, records[[3]]$content)
  expect_no_match(paste(texts, collapse = " "), "Be terse", fixed = TRUE)
})

test_that("a SystemTurn produces no records", {
  expect_equal(contents_trajectory(SystemTurn("Be terse.")), list())
})

test_that("tool result without a request falls back to a synthesized id", {
  turn <- UserTurn(list(ContentToolResult(value = "ok")))

  records <- contents_trajectory(turn, turn_index = 5)

  expect_length(records, 1)
  expect_equal(records[[1]]$role, "tool")
  expect_equal(records[[1]]$tool_call_id, "tool_5_1")
  expect_equal(records[[1]]$content, "ok")
})

test_that("timestamp is present only for assistant turns with created_at", {
  turn_with_time <- AssistantTurn("Hi!", json = list(created_at = 1700000000))
  turn_without_time <- AssistantTurn("Hi!")
  turn_user <- UserTurn("Hello")

  records_with_time <- contents_trajectory(turn_with_time)
  records_without_time <- contents_trajectory(turn_without_time)
  records_user <- contents_trajectory(turn_user)

  expect_named(records_with_time[[1]], c("role", "content", "timestamp"))
  expect_equal(records_with_time[[1]]$timestamp, format_iso8601(1700000000))

  expect_named(records_without_time[[1]], c("role", "content"))
  expect_named(records_user[[1]], c("role", "content"))
})

test_that("contents_trajectory works end to end for a Chat", {
  request <- ContentToolRequest("call_1", "lookup", list(x = 1))

  chat <- Chat$new(test_provider(model = "test-model-x"))
  chat$set_turns(list(
    UserTurn("What's the weather?"),
    AssistantTurn(list(request)),
    UserTurn(list(ContentToolResult(value = "Sunny", request = request))),
    AssistantTurn("It's sunny.")
  ))

  records <- contents_trajectory(chat)

  expect_equal(
    records[[1]],
    list(role = "meta", source = "ellmer", model = "test-model-x")
  )
  expect_length(records, 5)

  json <- jsonlite::toJSON(records, auto_unbox = TRUE)
  expect_type(as.character(json), "character")

  expect_valid_trajectory_records(records)
  expect_valid_against_schema(records)
})
