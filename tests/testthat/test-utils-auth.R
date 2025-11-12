# tests use `chat_openai()` to verify environments flow through correctly

test_that("api_key is deprecated", {
  expect_snapshot(chat <- chat_openai_compatible_test(api_key = "abc"))
  expect_equal(chat$get_provider()@credentials(), "Bearer abc")
})

test_that("errors if both credentials and api_key are provided", {
  expect_snapshot(
    chat_openai_compatible_test(credentials = "abc", api_key = "def"),
    error = TRUE
  )
})

test_that("verifies all properties of credentials", {
  expect_snapshot(error = TRUE, {
    chat_openai_test(credentials = 1)
    chat_openai_test(credentials = \(a, b) a + b)
    chat_openai_test(credentials = \() 1)
  })
})
