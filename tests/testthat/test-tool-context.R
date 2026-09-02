fake_ctx <- function(turns = list()) {
  structure(
    list(request = NULL, turns = turns),
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
  ctx <- fake_ctx(turns = list("a", "b"))

  helper <- function() {
    local_tool_context(ctx)
    list(
      turns = tool_context()$turns,
      request = tool_context()$request
    )
  }
  result <- helper()

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

test_that("tool context helpers reject NULL", {
  expect_error(with_tool_context(NULL, NULL), "context.*must be")
  expect_error(local_tool_context(NULL), "context.*must be")
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
  ctx_list <- list(request = NULL, turns = list("a"))

  result <- with_tool_context(ctx_list, tool_context())

  expect_s3_class(result, "ellmer_tool_context")
  expect_equal(result$turns, list("a"))
})

test_that("local_tool_context() auto-promotes a plain list", {
  ctx_list <- list(request = NULL, turns = list())

  helper <- function() {
    local_tool_context(ctx_list)
    tool_context()
  }
  result <- helper()

  expect_s3_class(result, "ellmer_tool_context")
})

test_that("as_tool_context() errors on non-list, non-context input", {
  expect_snapshot(with_tool_context("not a list", NULL), error = TRUE)
})

test_that("as_tool_context() fills in defaults for omitted fields", {
  result <- with_tool_context(list(), tool_context())
  expect_s3_class(result, "ellmer_tool_context")
  expect_null(result$request)
  expect_equal(result$turns, list())
})

test_that("new_tool_context() builds correct structure", {
  ctx <- new_tool_context(request = "req")
  expect_equal(class(ctx), "ellmer_tool_context")
  expect_identical(ctx$request, "req")
  expect_equal(ctx$turns, list())
})
