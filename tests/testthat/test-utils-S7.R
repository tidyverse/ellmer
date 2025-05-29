test_that("prop_whole_number validates inputs", {
  check_prop <- function(...) {
    new_class(
      "class",
      properties = list(prop = prop_number_whole(...)),
      package = NULL
    )
  }
  expect_snapshot(error = TRUE, {
    check_prop()("x")
    check_prop()(c(1:2))
    check_prop()(1.5)
    check_prop(min = 1)(0)
    check_prop(max = -1)(0)
  })
})

test_that("redacted values aren't saved to disk", {
  Test <- new_class("Test", properties = list(prop_redacted("redacted")))

  # Can get and set redacted values
  test <- Test(redacted = "secret")
  expect_equal(test@redacted, "secret")
  test@redacted <- "new secret"
  expect_equal(test@redacted, "new secret")

  # But can't save it to disk
  path <- withr::local_tempfile()
  saveRDS(test, path)
  test <- readRDS(path)
  expect_equal(test@redacted, NULL)
})
