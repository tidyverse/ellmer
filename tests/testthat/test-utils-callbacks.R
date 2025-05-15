test_that("CallbackManager catches argument mismatches", {
  callbacks <- CallbackManager$new(args = "data")

  expect_snapshot(error = TRUE, {
    callbacks$add("foo")
    callbacks$add(function(foo) NULL)
    callbacks$add(function(x, y) x + y)
  })

  expect_silent(callbacks$add(function(data) data))
  expect_silent(callbacks$invoke(data = data))
  expect_silent(callbacks$invoke(1))

  # Callbacks with invalid args throw standard R error
  expect_snapshot(error = TRUE, {
    callbacks$invoke()
    callbacks$invoke(1, 2)
  })
})

test_that("CallbackManager invokes callbacks in LIFO order", {
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
})

test_that("$add() returns a function to remove the callback", {
  callbacks <- CallbackManager$new()
  res1 <- NULL
  res2 <- NULL

  cb1 <- callbacks$add(function() {
    res1 <<- Sys.time()
  })
  cb2 <- callbacks$add(function() {
    res2 <<- Sys.time()
  })

  expect_equal(callbacks$count(), 2)
  callbacks$invoke()

  # Unregistering a callback
  res1_first <- res1
  res2_first <- res2
  cb1()
  expect_equal(callbacks$count(), 1)
  callbacks$invoke()
  expect_equal(res1, res1_first) # first callback result hasn't changed
  expect_true(res2 > res2_first) # second callback was evaluated

  # Unregistering callbacks are idempotent
  cb_list <- callbacks$get_callbacks()
  expect_null(cb_list[["1"]])
  cb1()
  # Callback list hasn't changed
  expect_equal(callbacks$get_callbacks(), cb_list)
})

test_that("$clear() clears all callbacks", {
  callbacks <- CallbackManager$new()
  res <- c()
  callbacks$add(function() res <<- c(res, 1))
  callbacks$add(function() res <<- c(res, 2))
  expect_equal(callbacks$count(), 2)

  callbacks$clear()
  expect_equal(callbacks$count(), 0)
  expect_equal(callbacks$get_callbacks(), list())

  # Invoking without registered callbacks means nothing happens
  expect_null(callbacks$invoke())
  expect_equal(res, c()) # nothing happened
})
