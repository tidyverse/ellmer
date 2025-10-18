test_that("checks inputs", {
  expect_snapshot(error = TRUE, {
    interpolate(1)
    interpolate("x", 1)
  })
})

test_that("vectorised interpolation generates a list", {
  expect_equal(
    interpolate("{{x}}", x = 1:2),
    ellmer_prompt(c("1", "2"))
  )
})

test_that("has a nice print method", {
  expect_snapshot(interpolate("Hi!"))
})

test_that("print method truncates many elements", {
  prompt <- ellmer_prompt(c("x\ny", c("a\nb\nc\nd\ne")))
  expect_snapshot({
    print(prompt, max_items = 1)
    print(prompt, max_lines = 2)
    print(prompt, max_lines = 3)
    print(ellmer_prompt("a\nb\nc\nd\ne"), max_lines = 3)
  })
})

test_that("can interpolate from local env or from ...", {
  x <- 1

  expect_equal(interpolate("{{x}}"), ellmer_prompt("1"))
  expect_equal(interpolate("{{x}}", x = 2), ellmer_prompt("2"))
})

test_that("can take a data frame via !!!", {
  df <- data.frame(x = 1, y = 2)
  expect_equal(interpolate("{{x}} + {{y}}", !!!df), ellmer_prompt("1 + 2"))
})

test_that("can interpolate from a file", {
  path <- withr::local_tempfile(lines = "{{x}}")
  expect_equal(interpolate_file(path, x = 1), ellmer_prompt("1"))

  # and errors if file doesn't exist
  expect_snapshot(interpolate_file("does-not-exist.md", x = 1), error = TRUE)
})

# interpolate_package() -------------------------------------------------------

test_that("can interpolate from a package", {
  inst_path <- withr::local_tempdir()
  local_mocked_bindings(inst_path = function(...) inst_path)

  dir.create(file.path(inst_path, "prompts"))
  con <- file(file.path(inst_path, "prompts", "bar.md"), "wb")
  writeLines("{{x}}", con)
  close(con)

  expect_equal(interpolate_package("test", "bar.md", x = 1), ellmer_prompt("1"))
})

test_that("informative errors if can't find prompts", {
  inst_path <- withr::local_tempdir()
  local_mocked_bindings(inst_path = function(...) inst_path)

  expect_snapshot(interpolate_package("x", "does-not-exist.md"), error = TRUE)

  dir.create(file.path(inst_path, "prompts"))
  expect_snapshot(interpolate_package("x", "does-not-exist.md"), error = TRUE)
})

test_that("checks its inputs", {
  expect_snapshot(error = TRUE, {
    interpolate_package(1)
    interpolate_package("x", 1)
  })
})
