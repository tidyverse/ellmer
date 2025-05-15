expect_record_replay <- function(
  x,
  ...,
  chat = chat_ollama_test("Be as terse as possible; no punctuation")
) {
  rlang::check_dots_empty()

  # Simulate the full bookmarking experience:
  # * Record the object to something serializable
  # * Serialize the object to JSON via shiny; "bookmark"
  # * Unserialize the object from JSON via shiny; "restore"
  # * Replay the unserialized object to the original object
  # * Check that the replayed object has the same class as the original object
  # * Check that the replayed object has the same properties as the original object

  # expect_silent({
  obj <- contents_record(x, chat = chat)

  # Bbookmark
  serialized <- shiny:::toJSON(obj)
  unserialized <- shiny:::safeFromJSON(serialized)

  replayed <- contents_replay(unserialized, chat = chat)
  # })

  expect_s3_class(replayed, class(x)[1])
  expect_equal(S7::props(replayed), S7::props(x))
}

# -------------------------------------------------------------------------

test_that("can round trip of Content record/replay", {
  expect_record_replay(Content())
})

test_that("can round trip of ContentText record/replay", {
  expect_record_replay(ContentText("hello world"))
})

test_that("can round trip of ContentImageInline record/replay", {
  expect_record_replay(
    ContentImageInline("image/png", "abcd123")
  )
})

test_that("can round trip of ContentImageRemote record/replay", {
  expect_record_replay(
    ContentImageRemote("https://example.com/image.jpg", detail = "")
  )
})

test_that("can round trip of ContentJson record/replay", {
  expect_record_replay(
    ContentJson(list(a = 1:2, b = "apple"))
  )
})

test_that("can round trip of ContentSql record/replay", {
  expect_record_replay(
    ContentSql("SELECT * FROM mtcars")
  )
})

test_that("can round trip of ContentSuggestions record/replay", {
  expect_record_replay(
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
  expect_record_replay(
    ContentThinking("A **thought**.")
  )
})

test_that("can round trip of ContentTool record/replay", {
  # TODO: barret - test tooldef, need to adjust replay to accept client to recontruct tooldef
  expect_record_replay(
    ContentToolRequest("ID", "tool_name", list(a = 1:2, b = "apple"))
  )
})

test_that("can round trip of ToolDef record/replay", {
  chat <- chat_ollama_test("Be as terse as possible; no punctuation")
  tool_rnorm <- tool(
    stats::rnorm,
    "Drawn numbers from a random normal distribution",
    n = type_integer("The number of observations. Must be a positive integer."),
    mean = type_number("The mean value of the distribution."),
    sd = type_number(
      "The standard deviation of the distribution. Must be a non-negative number."
    )
  )
  chat$register_tool(tool_rnorm)

  # with_chat(chat, {
  expect_record_replay(tool_rnorm, chat = chat)
  # })

  # with_chat(chat, {
  expect_record_replay(
    ContentToolRequest(
      "ID",
      "tool_name",
      list(a = 1:2, b = "apple"),
      tool = tool_rnorm
    ),
    chat = chat
  )
  # })
})

test_that("can round trip of ContentToolResult record/replay", {
  expect_record_replay(
    ContentToolResult(
      value = "VALUE",
      error = NULL,
      extra = list(extra = 1:2, b = "apple"),
      request = NULL
    )
  )
  # TODO: Barret test real error value
  # TODO: Barret test with request object
})

test_that("can round trip of ContentUploaded record/replay", {
  expect_record_replay(ContentUploaded("https://example.com/image.jpg"))
})

test_that("can round trip of ContentPDF record/replay", {
  expect_record_replay(ContentPDF(type = "TYPE", data = "DATA"))
})
