test_that("CallbackManager catches argument mismatches", {
  callbacks <- CallbackManager$new()

  expect_snapshot(error = TRUE, {
    callbacks$add("foo")
    callbacks$add(function(x, y) x + y)
    callbacks$add(function(x = 1, y) x + y)
  })

  expect_silent(callbacks$add(function(x, ...) x))
  expect_silent(callbacks$add(function(x, y = 1) x + y))

  # Callbacks require one argument, but throw standard R error
  expect_error(callbacks$invoke())
  expect_error(callbacks$invoke(1, 2))
})

test_that("CallbackManager invokes callbacks", {
  callbacks <- CallbackManager$new()
  res1 <- NULL
  res2 <- NULL

  cb1 <- callbacks$add(function(value) {
    res1 <<- list(value = value, time = Sys.time())
  })

  cb2 <- callbacks$add(function(...) {
    res <- list(time = Sys.time())
    res["value"] <- list(...)
    res2 <<- res
  })

  expect_equal(callbacks$count(), 2)

  # Callbacks don't return a value
  expect_null(callbacks$invoke(list(x = 1, y = 2)))

  # Callbacks receive expected arguments
  expect_equal(res1$value, list(x = 1, y = 2))
  expect_equal(res2$value, list(x = 1, y = 2))
  # Callbacks are invoked in reverse order
  expect_true(res1$time > res2$time)

  # Unregistering a callback
  res1_og <- res1
  cb1()
  expect_equal(callbacks$count(), 1)
  callbacks$invoke(list(x = 3, y = 4))
  expect_equal(res1, res1_og) # first callback result hasn't changed
  expect_equal(res2$value, list(x = 3, y = 4)) # second callback was evaluated

  callbacks$clear()
  expect_equal(callbacks$count(), 0)

  # Invoking without registered callbacks means nothing happens
  res2_og <- res2
  expect_null(callbacks$invoke(list(x = 5, y = 6)))
  expect_equal(res1, res1_og) # first callback wasn't evaluated
  expect_equal(res2, res2_og) # second callback also wasn't evaluated
})
