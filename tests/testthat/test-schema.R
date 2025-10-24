# df_schema -----------------------------------------------------------------

test_that("df_schema aggregates column descriptions", {
  df <- data.frame(x = 1:3, y = letters[1:3])
  expect_snapshot(df_schema(df))
})

test_that("df_schema works on edge case data frames", {
  expect_snapshot({
    df_schema(data.frame())
    df_schema(data.frame(x = 1, y = "a"))
    df_schema(data.frame(x = 1:5))
  })
})

test_that("df_schema checks its inputs", {
  expect_snapshot(error = TRUE, {
    df_schema(1)
  })
})

test_that("warns for wide data frames", {
  df <- vctrs::new_data_frame(set_names(rep(list(1), 26), letters))
  expect_snapshot(df_schema(df, max_cols = 5))
})

# col_schema ----------------------------------------------------------------

test_that("col_schema handles logical vectors", {
  expect_snapshot(col_schema(c(TRUE, FALSE, TRUE, NA)))
})

test_that("col_schema handles numeric vectors", {
  expect_snapshot({
    col_schema(c(1L:5L, NA))
    col_schema(c(1.5, 2.5, 3.5))
    col_schema(c(1.5, NA, NaN, Inf, -Inf, 3.5))
  })
})

test_that("col_schema handles characters and factors", {
  expect_snapshot({
    col_schema(c("a", "b", NA))
    col_schema(letters)
    col_schema(factor(c("a", "b", NA)))
    col_schema(factor(c("a", "b", NA), exclude = NULL))
    col_schema(ordered(c("low", "med", "high")))
  })
})

test_that("col_schema handles date/times", {
  expect_snapshot({
    col_schema(as.Date(c("2024-01-01", "2024-12-31", NA)))
    col_schema(as.POSIXct(c(
      "2024-01-01 10:30:00",
      "2024-12-31 23:59:59"
    )))
    col_schema(as.POSIXct(
      "2024-01-01 10:30:00",
      tz = "America/New_York"
    ))
  })
})

test_that("col_schema handles empty vectors", {
  expect_snapshot({
    col_schema(character(0))
    col_schema(numeric(0))
    col_schema(logical(0))
    col_schema(factor(character(0)))
    col_schema(as.Date(character(0)))
    col_schema(as.POSIXct(character(0)))
  })
})

test_that("col_schema handles data frame columns", {
  df <- data.frame(a = 1:3, b = letters[1:3])
  expect_snapshot(col_schema(df))
})

test_that("col_schema handles list columns", {
  lst <- list(1:3, letters[1:5], NULL)
  expect_snapshot(col_schema(lst))
})

test_that("col_schema handles unknown classes", {
  x <- structure(1:5, class = c("class1", "class2", "class3"))
  expect_snapshot(col_schema(x))
})

test_that("col_schema handles labeled columns", {
  x <- 1:5
  attr(x, "label") <- "My labeled variable"
  expect_snapshot(col_schema(x))
})

# desc_* ------------------------------------------------------------------

test_that("desc_range handles various numeric inputs", {
  expect_snapshot({
    desc_range(numeric())
    desc_range(c(-Inf, 1, 2, Inf))
    desc_range(c(NA, 1, 2, NA))
  })
})

test_that("desc_unique handles character vectors", {
  expect_snapshot({
    desc_unique(letters)
    desc_unique(strrep("x", 10000))
    desc_unique(character())
  })
})
