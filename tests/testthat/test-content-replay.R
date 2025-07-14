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

test_that("can round trip of ContentTool record/replay", {
  chat <- chat_openai_test()
  tool_rnorm <- tool(
    rnorm,
    description = "Drawn numbers from a random normal distribution",
    arguments = list(
      n = type_integer("The number of observations."),
      mean = type_number("The mean value of the distribution."),
      sd = type_number("The standard deviation of the distribution.")
    )
  )
  # Can round trip tooldef
  test_record_replay(tool_rnorm, chat = chat)
  chat$register_tool(tool_rnorm)

  # And request/result
  request_no_tool <- ContentToolRequest(
    "ID",
    "tool_name",
    list(a = 1:2, b = "apple")
  )
  request_tool <- ContentToolRequest(
    "ID",
    "tool_name",
    list(a = 1:2, b = "apple"),
    tool = tool_rnorm
  )
  result <- ContentToolResult(
    value = "VALUE",
    extra = list(extra = 1:2, b = "apple")
  )
  test_record_replay(request_no_tool, chat = chat)
  test_record_replay(request_tool, chat = chat)
  test_record_replay(result, chat = chat)
})


test_that("checks recorded value types", {
  chat <- chat_openai_test()

  bad_names <- list()
  bad_version <- list(version = 2, class = "ellmer::Content", props = list())
  bad_class <- list(version = 1, class = c("a", "b"), props = list())
  expect_snapshot(error = TRUE, {
    contents_replay(bad_names, chat = chat)
    contents_replay(bad_version, chat = chat)
    contents_replay(bad_class, chat = chat)
  })
})

test_that("non-ellmer classes are not recorded/replayed by default", {
  chat <- chat_openai_test()

  LocalClass <- S7::new_class("LocalClass", package = "foo")
  recorded <- list(version = 1, class = "foo::LocalClass", props = list())

  expect_snapshot(error = TRUE, {
    contents_record(LocalClass(), chat = chat)
    contents_replay(recorded, chat = chat)
  })
})

test_that("replayed objects must be existing S7 classes", {
  chat <- chat_openai_test()

  doesnt_exist <- list(version = 1, class = "ellmer::Turn2", props = list())
  not_s7 <- list(version = 1, class = "ellmer::chat_openai", props = list())

  expect_snapshot(error = TRUE, {
    contents_replay(doesnt_exist, chat = chat)
    contents_replay(not_s7, chat = chat)
  })
})
