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
  test_record_replay(Turn("user"))

  test_record_replay(Turn(
    "user",
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
  test_record_replay(ContentSql("SELECT * FROM mtcars"))
  test_record_replay(ContentThinking("A **thought**."))
  test_record_replay(ContentUploaded("https://example.com/image.jpg"))
  test_record_replay(ContentPDF(type = "TYPE", data = "DATA"))
})

test_that("can round trip of ContentSuggestions", {
  test_record_replay(
    ContentSuggestions(
      c(
        "What is the total quantity sold for each product last quarter?",
        "What is the average discount percentage for orders from the United States?",
        "What is the average price of products in the 'electronics' category?"
      )
    )
  )
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
  turn <- Turn("user", list(ContentToolRequest("123", "mytool")))
  recorded <- contents_record(turn)

  mytool <- tool(function() {}, "mytool")
  replayed <- contents_replay(recorded, tools = list(mytool = mytool))
  expect_equal(replayed@contents[[1]]@tool, mytool)

  # If no match, it still works, but tool is left as NULL
  replayed <- contents_replay(recorded, tools = list())
  expect_equal(replayed@contents[[1]]@tool, NULL)
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

test_that("replayed objects must be existing S7 classes", {
  doesnt_exist <- list(version = 1, class = "ellmer::Turn2", props = list())
  not_s7 <- list(version = 1, class = "ellmer::chat_openai", props = list())

  expect_snapshot(error = TRUE, {
    contents_replay(doesnt_exist)
    contents_replay(not_s7)
  })
})
