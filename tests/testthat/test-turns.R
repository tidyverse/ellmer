test_that("system prompt is applied correctly", {
  sys_prompt <- "foo"
  sys_msg <- Turn("system", sys_prompt)
  user_msg <- Turn("user", "bar")

  expect_equal(normalize_turns(), list())
  expect_equal(normalize_turns(list(user_msg)), list(user_msg))
  expect_equal(normalize_turns(list(sys_msg)), list(sys_msg))

  expect_equal(normalize_turns(list(), sys_prompt), list(sys_msg))
  expect_equal(
    normalize_turns(list(user_msg), sys_prompt),
    list(sys_msg, user_msg)
  )
  expect_equal(
    normalize_turns(list(sys_msg, user_msg), sys_prompt),
    list(sys_msg, user_msg)
  )
})

test_that("normalize_turns throws useful errors", {
  sys_prompt <- "foo"
  sys_msg <- Turn("system", "foo")
  user_msg <- Turn("user", "bar")

  expect_snapshot(error = TRUE, {
    normalize_turns(1)
    normalize_turns(list(1))
    normalize_turns(list(sys_msg, user_msg), 1)
    normalize_turns(list(sys_msg, user_msg), "foo2")
  })
})


test_that("as_user_turn gives useful errors", {
  expect_snapshot(error = TRUE, {
    as_user_turn(list())
    as_user_turn(list(x = 1))
    as_user_turn(1)
  })
})

test_that("can opt-out of empty check", {
  out <- as_user_turn(list(), check_empty = FALSE)
  expect_equal(out, Turn("user"))
})

test_that("as_user_turns gives useful errors", {
  x <- list(list(1))
  expect_snapshot(error = TRUE, {
    as_user_turns(1)
    as_user_turns(x)
  })
})

test_that("can extract text easily", {
  turn <- Turn(
    "assistant",
    list(
      ContentText("ABC"),
      ContentImage(),
      ContentText("DEF")
    )
  )
  expect_equal(turn@text, "ABCDEF")
})

test_that("turns have a reasonable print method", {
  expect_snapshot(Turn("user", "hello"))
})

test_that("as_user_turns can create lists of turns from lists of Content objects", {
  content_turns <- as_user_turns(
    list(
      content_image_url("https://www.r-project.org/Rlogo.png"),
      content_image_url("https://www.r-project.org/Rlogo.png")
    )
  )

  expect_length(content_turns, 2)
  expect_s3_class(content_turns[[1]], "ellmer::Turn")
  expect_s3_class(content_turns[[2]], "ellmer::Turn")
})

# turn_contents_preview()

test_that("ContentText shows first (truncated) text", {
  expect_equal(
    turn_contents_preview(Turn("user", "This is short")),
    "Text[This is short]"
  )
  expect_equal(
    turn_contents_preview(Turn(
      "user",
      "This is a very long message that should be truncated"
    )),
    "Text[This is a very long message that shou...]"
  )

  expect_equal(
    turn_contents_preview(Turn(
      "user",
      list(ContentText("This is short"), ContentText("Second"))
    )),
    "Text[This is short], Text"
  )
})

test_that("non-text types just show type", {
  expect_equal(
    turn_contents_preview(Turn("user", list(ContentImageInline("")))),
    "ImageInline"
  )

  expect_equal(
    turn_contents_preview(Turn(
      "user",
      list(ContentImageInline(""), ContentImageRemote(""))
    )),
    "ImageInline, ImageRemote"
  )
})
