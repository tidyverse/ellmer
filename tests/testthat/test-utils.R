test_that("finds key if set", {
  withr::local_envvar(FOO = "abc123")
  expect_true(key_exists("FOO"))
  expect_equal(key_get("FOO"), "abc123")
})


test_that("informative error if no key", {
  withr::local_envvar(FOO = NULL, TESTTHAT = "false")
  expect_false(key_exists("FOO"))
  expect_snapshot(key_get("FOO"), error = TRUE)
})

test_that("detects whitespace", {
  expect_true(is_whitespace("\n\n\n \t"))
  expect_true(is_whitespace(""))

  expect_false(is_whitespace("a"))
  expect_false(is_whitespace("."))
})

test_that('echo="output" replaces echo="text"', {
  expect_snapshot(
    expect_equal(check_echo("text"), "output")
  )
})
