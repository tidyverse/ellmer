# Object from ContentJSON -----------------------------------------------------

test_that("useful error if no ContentJson", {
  turn <- Turn("assistant", list(ContentText("Hello")))
  expect_snapshot(extract_data(turn), error = TRUE)
})

test_that("can extract data from ContentJson", {
  turn <- Turn("assistant", list(ContentJson(list(x = 1))))
  type <- type_object(x = type_integer())
  expect_equal(extract_data(turn, type), list(x = 1))
})

test_that("can opt-out of conversion data from ContentJson", {
  turn <- Turn("assistant", list(ContentJson(list(x = list(1, 2)))))
  type <- type_object(x = type_array(items = type_integer()))
  expect_equal(extract_data(turn, type, convert = TRUE), list(x = c(1L, 2L)))
  expect_equal(extract_data(turn, type, convert = FALSE), list(x = list(1, 2)))
})

test_that("can extract data when wrapper is used", {
  turn <- Turn("assistant", list(ContentJson(list(wrapper = list(x = 1)))))
  type <- wrap_type_if_needed(type_object(x = type_integer()), TRUE)
  expect_equal(extract_data(turn, type, needs_wrapper = TRUE), list(x = 1))
})

# Type coercion ---------------------------------------------------------------

test_that("optional base types (scalars) stay as NULL", {
  expect_equal(convert_from_type(NULL, type_boolean(required = FALSE)), NULL)
  expect_equal(convert_from_type(NULL, type_integer(required = FALSE)), NULL)
  expect_equal(convert_from_type(NULL, type_number(required = FALSE)), NULL)
  expect_equal(convert_from_type(NULL, type_string(required = FALSE)), NULL)
})

test_that("can convert arrays of basic types to simple vectors", {
  expect_equal(
    convert_from_type(list(FALSE, TRUE), type_array(items = type_boolean())),
    c(FALSE, TRUE)
  )
  expect_identical(
    convert_from_type(list(1, 2), type_array(items = type_integer())),
    c(1L, 2L)
  )
  expect_equal(
    convert_from_type(list(1.2, 2.5), type_array(items = type_number())),
    c(1.2, 2.5)
  )
  expect_equal(
    convert_from_type(list("x", "y"), type_array(items = type_string())),
    c("x", "y")
  )
})

test_that("handles empty and NULL vectors of basic types", {
  type <- type_array(items = type_boolean(required = FALSE))
  expect_equal(convert_from_type(list(FALSE, TRUE), type), c(FALSE, TRUE))
  expect_equal(convert_from_type(list(NULL, TRUE), type), c(NA, TRUE))

  type <- type_array(items = type_integer(required = FALSE))
  expect_identical(convert_from_type(list(), type), integer())
  expect_identical(convert_from_type(list(NULL), type), NA_integer_)

  type <- type_array(items = type_number(required = FALSE))
  expect_identical(convert_from_type(list(), type), double())
  expect_identical(convert_from_type(list(NULL), type), NA_real_)

  type <- type_array(items = type_string(required = FALSE))
  expect_equal(convert_from_type(list(), type), character())
  expect_equal(convert_from_type(list(NULL), type), NA_character_)
})

test_that("completely missing optional components become NULL", {
  type <- type_integer(required = FALSE)
  expect_equal(convert_from_type(NULL, type), NULL)

  type <- type_array(items = type_integer(), required = FALSE)
  expect_equal(convert_from_type(NULL, type), NULL)

  type <- type_object(
    x = type_integer(),
    y = type_integer(required = FALSE)
  )
  expect_equal(
    convert_from_type(list(x = 1), type),
    list(x = 1, y = NULL)
  )
})

test_that("can handle missing optional values in objects (#384)", {
  data <- list(
    list(fruit = "Apples", year = NULL),
    list(fruit = "Oranges", year = NULL)
  )
  type <- type_array(
    items = type_object(
      fruit = type_string(),
      year = type_integer(required = FALSE)
    )
  )
  expect_equal(
    convert_from_type(data, type),
    data.frame(
      fruit = c("Apples", "Oranges"),
      year = c(NA_integer_, NA_integer_)
    )
  )
})

test_that("can covert array of arrays to lists of vectors", {
  expect_equal(
    convert_from_type(
      list(list(1, 2), list(3, 4)),
      type_array(items = type_array(items = type_integer()))
    ),
    list(c(1L, 2L), c(3L, 4L))
  )
})

test_that("can convert arrays of enums to factors", {
  expect_equal(
    convert_from_type(
      list("x", "y"),
      type_array(items = type_enum(values = c("x", "y", "z")))
    ),
    factor(c("x", "y"), levels = c("x", "y", "z"))
  )
})

test_that("can convert arrays of objects to data frames", {
  expect_equal(
    convert_from_type(
      list(list(x = 1, y = "x"), list(x = 3, y = "y")),
      type_array(
        items = type_object(
          x = type_integer(),
          y = type_string()
        )
      )
    ),
    data.frame(x = c(1L, 3L), y = c("x", "y"))
  )
})

test_that("can recursively convert objects contents", {
  expect_equal(
    convert_from_type(
      list(x = 1, y = list(1, 2, 3)),
      type_object(
        x = type_integer(),
        y = type_array(items = type_integer())
      )
    ),
    list(x = 1, y = c(1, 2, 3))
  )
})
