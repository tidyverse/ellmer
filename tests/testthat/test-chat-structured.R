test_that("structured data is round-tripped", {
  chat <- chat_openai_test()
  data <- chat$chat_structured(
    "Generate the name and age of a random person.",
    type = type_object(
      name = type_string(),
      age = type_number()
    )
  )
  expect_match(chat$chat("What is the name of the person?"), data$name)
})

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
  type <- type_object(x = type_array(type_integer()))
  expect_equal(extract_data(turn, type, convert = TRUE), list(x = c(1L, 2L)))
  expect_equal(extract_data(turn, type, convert = FALSE), list(x = list(1, 2)))
})

test_that("can extract data when wrapper is used", {
  turn <- Turn("assistant", list(ContentJson(list(wrapper = list(x = 1)))))
  type <- wrap_type_if_needed(type_object(x = type_integer()), TRUE)
  expect_equal(extract_data(turn, type, needs_wrapper = TRUE), list(x = 1))
})

test_that("warns if multiple ContentJson (and uses first)", {
  turn <- Turn(
    "assistant",
    list(
      ContentJson(list(x = 1)),
      ContentJson(list(x = 2)),
      ContentJson(list(x = 3))
    )
  )
  type <- type_object(x = type_integer())
  expect_snapshot(result <- extract_data(turn, type))
  expect_equal(result, list(x = 1))
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
    convert_from_type(list(FALSE, TRUE), type_array(type_boolean())),
    c(FALSE, TRUE)
  )
  expect_identical(
    convert_from_type(list(1L, 2L), type_array(type_integer())),
    c(1L, 2L)
  )
  expect_identical(
    convert_from_type(list(1, 2), type_array(type_integer())),
    c(1L, 2L)
  )
  expect_identical(
    convert_from_type(list(1L, 2L), type_array(type_number())),
    c(1, 2)
  )
  expect_equal(
    convert_from_type(list(1.2, 2.5), type_array(type_number())),
    c(1.2, 2.5)
  )
  expect_equal(
    convert_from_type(list("x", "y"), type_array(type_string())),
    c("x", "y")
  )
})

test_that("values of wrong type are silently converted to NA", {
  expect_equal(
    convert_from_type(list(1.2, "x"), type_array(type_number())),
    c(1.2, NA)
  )
})

test_that("values of incorrect length silently truncated", {
  expect_equal(
    convert_from_type(list(c(1, 2), c()), type_array(type_number())),
    c(1, NA)
  )
})

test_that("handles empty and NULL vectors of basic types", {
  type <- type_array(type_boolean(required = FALSE))
  expect_equal(convert_from_type(list(FALSE, TRUE), type), c(FALSE, TRUE))
  expect_equal(convert_from_type(list(NULL, TRUE), type), c(NA, TRUE))

  type <- type_array(type_integer(required = FALSE))
  expect_identical(convert_from_type(list(), type), integer())
  expect_identical(convert_from_type(list(NULL), type), NA_integer_)

  type <- type_array(type_number(required = FALSE))
  expect_identical(convert_from_type(list(), type), double())
  expect_identical(convert_from_type(list(NULL), type), NA_real_)

  type <- type_array(type_string(required = FALSE))
  expect_equal(convert_from_type(list(), type), character())
  expect_equal(convert_from_type(list(NULL), type), NA_character_)
})

test_that("scalar enums are converted to strings", {
  type <- type_enum(c("A", "B", "C"))
  expect_equal(convert_from_type("A", type), "A")
})


test_that("completely missing optional components become NULL", {
  type <- type_integer(required = FALSE)
  expect_equal(convert_from_type(NULL, type), NULL)

  type <- type_array(type_integer(), required = FALSE)
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

test_that("objects take order from type", {
  x <- list(y = 1, x = 2)
  type1 <- type_object(x = type_integer(), y = type_integer())
  expect_equal(convert_from_type(x, type1), list(x = 2, y = 1))

  type2 <- type_object(x = type_integer(), .additional_properties = TRUE)
  expect_equal(convert_from_type(x, type2), list(x = 2, y = 1))
})

test_that("additional properties are ignored, unless specified by type", {
  x <- list(y = 1, x = 2, z = 3)
  type <- type_object(x = type_integer())
  expect_equal(convert_from_type(x, type), list(x = 2))

  type <- type_object(x = type_integer(), .additional_properties = TRUE)
  expect_equal(convert_from_type(x, type), list(x = 2, y = 1, z = 3))
})

test_that("can handle missing optional values in objects (#384)", {
  data <- list(
    list(fruit = "Apples", year = NULL),
    list(fruit = "Oranges", year = NULL)
  )
  type <- type_array(
    type_object(
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
      type_array(type_array(type_integer()))
    ),
    list(c(1L, 2L), c(3L, 4L))
  )
})

test_that("arrays of enums are converted to factors", {
  type <- type_array(type_enum(c("x", "y", "z")))
  expect_equal(
    convert_from_type(list("x", "y"), type),
    factor(c("x", "y"), levels = c("x", "y", "z"))
  )
})

test_that("can convert arrays of objects to data frames", {
  x <- list(list(x = 1, y = "x"), list(x = 3, y = "y"))
  type <- type_array(type_object(x = type_integer(), y = type_string()))
  expect_equal(
    convert_from_type(x, type),
    data.frame(x = c(1L, 3L), y = c("x", "y"))
  )

  # unless they have additional properties
  type <- type_array(type_object(
    x = type_integer(),
    .additional_properties = TRUE
  ))
  expect_equal(convert_from_type(x, type), x)

  # in which case the order should still be preserved
  type2 <- type_array(type_object(
    y = type_integer(),
    .additional_properties = TRUE
  ))
  expect_equal(
    convert_from_type(x, type2),
    list(list(y = "x", x = 1), list(y = "y", x = 3))
  )
})

test_that("array of object with nested objects becomes packed data frame", {
  type <- type_array(
    type_object(
      x = type_object(a = type_integer()),
      y = type_object(a = type_integer())
    )
  )

  data <- list(
    list(x = list(a = 1), y = list(a = 3)),
    list(x = list(a = 5), y = list(a = 7))
  )

  out <- convert_from_type(data, type)
  expect_equal(nrow(out), 2)
  expect_named(out, c("x", "y"))
  expect_equal(out$x, data.frame(a = c(1, 5)))
  expect_equal(out$y, data.frame(a = c(3, 7)))
})

test_that("can recursively convert objects contents", {
  expect_equal(
    convert_from_type(
      list(x = 1, y = list(1, 2, 3)),
      type_object(x = type_integer(), y = type_array(type_integer()))
    ),
    list(x = 1, y = c(1, 2, 3))
  )
})
