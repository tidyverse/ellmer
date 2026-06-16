fake_ctx <- function(store = new.env(parent = emptyenv()), turns = list()) {
  structure(
    list(request = NULL, store = store, turns = turns),
    class = "ellmer_tool_context"
  )
}

test_that("tool_context() outside a tool gives informative error", {
  expect_snapshot(tool_context(), error = TRUE)
})

test_that("local_tool_context() makes tool_context() return the context", {
  ctx <- fake_ctx()
  helper <- function() {
    local_tool_context(ctx)
    tool_context()
  }
  result <- helper()
  expect_identical(result, ctx)
})

test_that("tool_context() fields are accessible via local_tool_context()", {
  store <- new.env(parent = emptyenv())
  store$x <- 42L
  ctx <- fake_ctx(store = store, turns = list("a", "b"))

  helper <- function() {
    local_tool_context(ctx)
    list(
      store = tool_context()$store,
      turns = tool_context()$turns,
      request = tool_context()$request
    )
  }
  result <- helper()

  expect_identical(result$store, store)
  expect_equal(result$turns, list("a", "b"))
  expect_null(result$request)
})

test_that("local_tool_context() pops when the frame exits", {
  helper <- function() {
    local_tool_context(fake_ctx())
  }
  helper()
  expect_equal(length(the$tool_context_stack), 0L)
})

test_that("with_tool_context() pops after normal exit", {
  ctx <- fake_ctx()
  with_tool_context(ctx, NULL)
  expect_equal(length(the$tool_context_stack), 0L)
})

test_that("with_tool_context() pops after an error in code", {
  ctx <- fake_ctx()
  tryCatch(
    with_tool_context(ctx, stop("boom")),
    error = function(e) NULL
  )
  expect_equal(length(the$tool_context_stack), 0L)
})

test_that("with_tool_context() returns the value of code", {
  ctx <- fake_ctx()
  result <- with_tool_context(ctx, 42L)
  expect_equal(result, 42L)
})

test_that("nested with_tool_context(): inner top wins; depth restored", {
  outer_ctx <- fake_ctx()
  inner_ctx <- fake_ctx()

  outer_top <- NULL
  inner_top <- NULL

  with_tool_context(outer_ctx, {
    outer_top <- tool_context()
    with_tool_context(inner_ctx, {
      inner_top <- tool_context()
    })
    expect_equal(length(the$tool_context_stack), 1L)
  })

  expect_identical(outer_top, outer_ctx)
  expect_identical(inner_top, inner_ctx)
  expect_equal(length(the$tool_context_stack), 0L)
})

test_that("with_tool_context() auto-promotes a plain list", {
  store <- new.env(parent = emptyenv())
  store$x <- 1L
  ctx_list <- list(request = NULL, store = store, turns = list("a"))

  result <- with_tool_context(ctx_list, tool_context())

  expect_s3_class(result, "ellmer_tool_context")
  expect_identical(result$store, store)
  expect_equal(result$turns, list("a"))
})

test_that("local_tool_context() auto-promotes a plain list", {
  store <- new.env(parent = emptyenv())
  ctx_list <- list(request = NULL, store = store, turns = list())

  helper <- function() {
    local_tool_context(ctx_list)
    tool_context()
  }
  result <- helper()

  expect_s3_class(result, "ellmer_tool_context")
  expect_identical(result$store, store)
})

test_that("as_tool_context() errors on non-list, non-context input", {
  expect_snapshot(with_tool_context("not a list", NULL), error = TRUE)
})

test_that("as_tool_context() fills in defaults for omitted fields", {
  result <- with_tool_context(list(), tool_context())
  expect_s3_class(result, "ellmer_tool_context")
  expect_null(result$request)
  expect_type(result$store, "environment")
  expect_equal(result$turns, list())
})

test_that("new_tool_context() builds correct structure", {
  store <- new.env(parent = emptyenv())
  ctx <- new_tool_context(request = "req", store = store, turns = list())
  expect_equal(class(ctx), "ellmer_tool_context")
  expect_identical(ctx$request, "req")
  expect_identical(ctx$store, store)
  expect_equal(ctx$turns, list())
})

test_that("tool_context_factory() builds contexts per request", {
  store <- new.env(parent = emptyenv())
  turns <- list("turn1")
  factory <- tool_context_factory(store, turns)

  ctx1 <- factory("req1")
  ctx2 <- factory("req2")

  expect_equal(class(ctx1), "ellmer_tool_context")
  expect_identical(ctx1$request, "req1")
  expect_identical(ctx2$request, "req2")
  expect_identical(ctx1$store, store)
  expect_identical(ctx1$store, ctx2$store)
})
