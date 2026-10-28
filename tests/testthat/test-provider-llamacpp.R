test_that("can list models loaded in llama.cpp server", {
  skip_if_no_llamacpp()
  models <- models_llamacpp()
  expect_s3_class(models, "data.frame")
  expect_named(models, c("id", "created_at", "owned_by"))
})

test_that("has_llamacpp is FALSE when server not running", {
  # use a port that should not be serving llama.cpp
  expect_false(has_llamacpp(base_url = "http://localhost:1"))
})
