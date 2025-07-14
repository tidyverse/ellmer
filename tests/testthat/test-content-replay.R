# -------------------------------------------------------------------------

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


test_that("can round trip of Content record/replay", {
  test_record_replay(Content())
})

test_that("can round trip of ContentText record/replay", {
  test_record_replay(ContentText("hello world"))
})

test_that("can round trip of ContentImageInline record/replay", {
  test_record_replay(
    ContentImageInline("image/png", "abcd123")
  )
})

test_that("can round trip of ContentImageRemote record/replay", {
  test_record_replay(
    ContentImageRemote("https://example.com/image.jpg", detail = "")
  )
})

test_that("can round trip of ContentJson record/replay", {
  test_record_replay(
    ContentJson(list(a = 1:2, b = "apple"))
  )
})

test_that("can round trip of ContentSql record/replay", {
  test_record_replay(
    ContentSql("SELECT * FROM mtcars")
  )
})

test_that("can round trip of ContentSuggestions record/replay", {
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

test_that("can round trip of ContentThinking record/replay", {
  test_record_replay(
    ContentThinking("A **thought**.")
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
  chat$register_tool(tool_rnorm)

  test_record_replay(
    ContentToolRequest("ID", "tool_name", list(a = 1:2, b = "apple")),
    chat = chat
  )
})

test_that("can round trip of ToolDef record/replay", {
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
  test_record_replay(tool_rnorm, chat = chat)

  chat$register_tool(tool_rnorm)
  test_record_replay(
    ContentToolRequest(
      "ID",
      "tool_name",
      list(a = 1:2, b = "apple"),
      tool = tool_rnorm
    ),
    chat = chat
  )

  recorded_tool <- contents_record(tool_rnorm, chat = chat)
  chat_empty <- chat_openai_test()
  replayed_tool <- contents_replay(recorded_tool, chat = chat_empty)

  tool_rnorm_empty <- ToolDef(
    # rnorm,
    name = "rnorm",
    description = "Drawn numbers from a random normal distribution",
    arguments = type_object(
      n = type_integer("The number of observations."),
      mean = type_number("The mean value of the distribution."),
      sd = type_number("The standard deviation of the distribution.")
    ),
  )

  expect_equal(replayed_tool, tool_rnorm_empty)
})

test_that("can round trip of ContentToolResult record/replay", {
  test_record_replay(
    ContentToolResult(
      value = "VALUE",
      error = NULL,
      extra = list(extra = 1:2, b = "apple"),
      request = NULL
    )
  )

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
  chat$register_tool(tool_rnorm)

  replayed <-
    test_record_replay(
      ContentToolResult(
        value = "VALUE",
        error = try(stop("boom"), silent = TRUE),
        extra = list(extra = 1:2, b = "apple"),
        request = ContentToolRequest(
          "ID",
          "tool_name",
          list(a = 1:2, b = "apple"),
          tool = tool_rnorm
        )
      ),
      chat = chat
    )

  tryCatch(
    signalCondition(replayed@error), # re-throw error
    error = function(e) {
      expect_equal(
        e$message,
        "boom"
      )
    }
  )
})

test_that("can round trip of ContentUploaded record/replay", {
  test_record_replay(ContentUploaded("https://example.com/image.jpg"))
})

test_that("can round trip of ContentPDF record/replay", {
  test_record_replay(ContentPDF(type = "TYPE", data = "DATA"))
})

test_that("non-ellmer classes are not recorded/replayed by default", {
  chat <- chat_openai_test()

  LocalClass <- S7::new_class(
    "LocalClass",
    properties = list(
      name = prop_string()
    ),
    # Make sure to unset the package being used!
    # Within testing, it sets the package to "ellmer"
    package = NULL
  )

  expect_snapshot(
    contents_record(LocalClass("testname"), chat = chat),
    error = TRUE
  )
  expect_snapshot(
    contents_replay(
      list(
        version = 1,
        class = "testpkg::LocalClass",
        props = list(name = "testname")
      ),
      chat = chat
    ),
    error = TRUE
  )
})

test_that("unknown classes cause errors", {
  chat <- chat_openai_test()
  recorded <- contents_record(Turn("user"), chat = chat)
  recorded$class <- "ellmer::Turn2"

  expect_error(
    contents_replay(recorded, chat = chat),
    "Unable to find the S7 class"
  )

  expect_snapshot(contents_replay(recorded, chat = chat), error = TRUE)
})

test_that("replay classes are S7 classes", {
  OtherName <- S7::new_class(
    "LocalClass",
    properties = list(
      name = prop_string()
    ),
    # Make sure to unset the package being used!
    # Within testing, it sets the package to "ellmer"
    package = NULL
  )
  LocalClass <- function(name) {
    OtherName(name = name)
  }

  chat <- chat_openai_test()
  recorded <- contents_record(LocalClass("testname"), chat = chat)
  expect_error(
    contents_replay(recorded, chat = chat),
    "is not an S7 class"
  )

  expect_snapshot(contents_replay(recorded, chat = chat), error = TRUE)
})
