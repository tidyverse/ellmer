test_that("can serialize simple objects", {
  expect_equal(
    contents_record(Content()),
    recorded_object("ellmer::Content", set_names(list()))
  )

  expect_equal(
    contents_record(ContentText("hello world")),
    recorded_object("ellmer::ContentText", list(text = "hello world"))
  )
})

test_that("can round trip of Turn record/replay", {
  test_record_replay(UserTurn())

  test_record_replay(UserTurn(
    list(
      ContentText("hello world"),
      ContentText("hello world2")
    )
  ))
})

test_that("can round trip simple content types", {
  test_record_replay(Content())
  test_record_replay(ContentText("hello world"))
  test_record_replay(ContentImageInline("image/png", "abcd123"))
  test_record_replay(ContentImageRemote("https://example.com/image.jpg"))
  test_record_replay(ContentJson(list(a = 1:2, b = "apple")))
  test_record_replay(ContentThinking("A **thought**."))
  test_record_replay(ContentUploaded("https://example.com/image.jpg"))
  test_record_replay(ContentPDF("TYPE", "DATA", "FILENAME"))
  test_record_replay(ContentDocument(
    "text/csv",
    "DATA",
    "FILE.csv",
    url = "https://example.com/f.csv"
  ))
  test_record_replay(WebSource("https://example.com", "Example"))
  test_record_replay(
    ContentCitation(
      source = WebSource("https://example.com", "Example"),
      grounded_span = "answer text",
      cited_quote = "source text",
      extra = list(id = "citation-1")
    )
  )
  test_record_replay(
    ContentToolRequestSearch(
      "ellmer",
      extra = list(type = "search")
    )
  )
  test_record_replay(
    ContentToolResponseSearch(
      list(WebSource("https://example.com", "Example")),
      extra = list(type = "search_results")
    )
  )
  test_record_replay(
    ContentToolRequestFetch(
      "https://example.com",
      extra = list(type = "fetch")
    )
  )
  test_record_replay(
    ContentToolResponseFetch(
      "https://example.com",
      status = "success",
      extra = list(type = "fetch_result")
    )
  )
})

test_that("ContentUploaded round trips with provider and extra", {
  test_record_replay(
    ContentUploaded(
      "file_123",
      mime_type = "application/pdf",
      provider = "anthropic",
      extra = list(filename = "report.pdf", size_bytes = 123)
    )
  )

  # recordings made before provider/extra existed still replay
  recorded <- recorded_object(
    "ellmer::ContentUploaded",
    list(uri = "file_123", mime_type = "application/pdf")
  )
  replayed <- contents_replay(recorded)
  expect_equal(replayed@provider, "")
  expect_equal(replayed@extra, list())
})

test_that("can round trip of ContentToolRequest/ContentToolResult", {
  request <- ContentToolRequest(
    "ID",
    "tool_name",
    list(a = 1:2, b = "apple")
  )
  result <- ContentToolResult(
    value = "VALUE",
    extra = list(extra = 1:2, b = "apple")
  )
  test_record_replay(request)
  test_record_replay(result)
})

test_that("can re-match tools if present", {
  turn <- UserTurn(list(ContentToolRequest("123", "mytool")))
  recorded <- contents_record(turn)

  mytool <- tool(function() {}, "mytool")
  replayed <- contents_replay(recorded, tools = list(mytool = mytool))
  expect_equal(replayed@contents[[1]]@tool, mytool)

  # If no match, it still works, but tool is left as NULL
  replayed <- contents_replay(recorded, tools = list())
  expect_equal(replayed@contents[[1]]@tool, NULL)
})

test_that("can re-match tools if present", {
  mytool <- tool(function() {}, "mytool")

  request <- ContentToolRequest("123", "mytool", tool = mytool)
  result <- ContentToolResult("value", request = request)

  turn_request <- UserTurn(list(request))
  turn_result <- UserTurn(list(result))

  test_record_replay(turn_request, tools = list(mytool = mytool))
  test_record_replay(turn_result, tools = list(mytool = mytool))

  # If no tool match, it still works, but tool is left as NULL
  replayed_turn_request <- contents_replay(
    contents_record(turn_request),
    tools = list()
  )
  expect_null(replayed_turn_request@contents[[1]]@tool)

  replayed_turn_result <- contents_replay(
    contents_record(turn_result),
    tools = list()
  )
  expect_null(replayed_turn_result@contents[[1]]@request@tool)
})

test_that("provider annotations replay only to their native provider", {
  providers <- list(
    anthropic = chat_anthropic_test()$get_provider(),
    openai = chat_openai_test()$get_provider(),
    google = chat_google_gemini_test()$get_provider()
  )
  annotations <- list(
    anthropic = ContentToolRequestSearch(
      "anthropic query",
      extra = list(
        type = "server_tool_use",
        id = "anthropic-marker",
        name = "web_search",
        input = list(query = "anthropic query")
      )
    ),
    openai = ContentToolRequestSearch(
      "openai query",
      extra = list(
        type = "web_search_call",
        id = "openai-marker",
        action = list(type = "search", query = "openai query")
      )
    ),
    google = ContentToolRequestSearch(
      "google query",
      extra = list(
        webSearchQueries = list("google-marker")
      )
    )
  )

  for (target in names(providers)) {
    for (source in names(annotations)) {
      turn <- AssistantTurn(
        list(
          ContentText("answer"),
          annotations[[source]],
          ContentCitation(
            grounded_span = "answer",
            extra = list(id = "citation-marker")
          )
        )
      )
      json <- jsonlite::toJSON(
        as_json(providers[[target]], turn),
        auto_unbox = TRUE
      )

      expect_false(grepl("citation-marker", json, fixed = TRUE))
      expect_equal(
        grepl(paste0(source, "-marker"), json, fixed = TRUE),
        target == source && target %in% c("anthropic", "openai"),
        info = paste("target:", target, "source:", source)
      )
    }
  }
})

test_that("Anthropic replays native search and fetch result blocks", {
  provider <- chat_anthropic_test()$get_provider()
  blocks <- list(
    ContentToolResponseSearch(
      sources = list(WebSource("https://example.com", "Example")),
      extra = list(
        type = "web_search_tool_result",
        tool_use_id = "search-1",
        content = list()
      )
    ),
    ContentToolResponseFetch(
      url = "https://example.com",
      status = "success",
      extra = list(
        type = "web_fetch_tool_result",
        tool_use_id = "fetch-1",
        content = list(type = "web_fetch_result")
      )
    )
  )

  expect_identical(as_json(provider, blocks[[1]]), blocks[[1]]@extra)
  expect_identical(as_json(provider, blocks[[2]]), blocks[[2]]@extra)
})

test_that("checks recorded value types", {
  bad_names <- list()
  bad_version <- list(version = 2, class = "ellmer::Content", props = list())
  bad_class <- list(version = 1, class = c("a", "b"), props = list())
  expect_snapshot(error = TRUE, {
    contents_replay(bad_names)
    contents_replay(bad_version)
    contents_replay(bad_class)
  })
})

test_that("non-ellmer classes are not recorded/replayed by default", {
  LocalClass <- S7::new_class("LocalClass", package = "foo")
  recorded <- list(version = 1, class = "foo::LocalClass", props = list())

  expect_snapshot(error = TRUE, {
    contents_record(LocalClass())
    contents_replay(recorded)
  })
})

test_that("packaged classes that extend ellmer classes can be replayed", {
  env <- rlang::current_env()

  local_mocked_bindings(
    ns_env = function(x) {
      if (x == "foo") {
        return(env)
      }
      rlang::ns_env(x)
    }
  )

  LocalContentText <- S7::new_class(
    "LocalContentText",
    package = "foo",
    parent = ellmer::ContentText
  )

  test_record_replay(LocalContentText("hello world"))
  test_record_replay(
    UserTurn(list(LocalContentText(text = "hello world")))
  )
})

test_that("local classes that extend ellmer classes can be replayed", {
  LocalContentText <- S7::new_class(
    "LocalContentText",
    package = NULL,
    parent = ellmer::ContentText
  )

  test_record_replay(LocalContentText("hello world"))
  test_record_replay(
    UserTurn(list(LocalContentText(text = "hello world")))
  )
})

test_that("local classes that extend ellmer classes can be replayed", {
  test_content <- S7::new_class(
    "LocalContentText",
    package = NULL,
    parent = ellmer::ContentText
  )

  LocalContentText <- function(...) {
    test_content(...)
  }

  expect_snapshot(
    error = TRUE,
    test_record_replay(test_content("hello world"))
  )
})

test_that("replayed objects must be existing S7 classes", {
  doesnt_exist <- list(version = 1, class = "ellmer::Turn2", props = list())
  not_s7 <- list(version = 1, class = "ellmer::chat_openai", props = list())

  expect_snapshot(error = TRUE, {
    contents_replay(doesnt_exist)
    contents_replay(not_s7)
  })
})
