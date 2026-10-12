test_that("list_llamacpp_models finds GGUF files by magic bytes", {
  dir <- withr::local_tempdir()
  writeBin(c(charToRaw("GGUF"), as.raw(1:100)), file.path(dir, "model1.bin"))
  writeBin(charToRaw("not a model"), file.path(dir, "notes.txt"))
  writeBin(c(charToRaw("GGUF"), as.raw(1:50)), file.path(dir, "weights.gguf"))

  res <- list_llamacpp_models(dirs = dir)

  expect_s3_class(res, "data.frame")
  expect_named(res, c("name", "path", "size"))
  expect_equal(nrow(res), 2)
  expect_true(all(c("model1.bin", "weights.gguf") %in% res$name))
  expect_true(all(file.exists(res$path)))
})

test_that("list_llamacpp_models returns empty data frame when nothing found", {
  res <- list_llamacpp_models(dirs = withr::local_tempdir())
  expect_equal(nrow(res), 0)
  expect_named(res, c("name", "path", "size"))

  # non-existent dirs are skipped silently
  res2 <- list_llamacpp_models(dirs = file.path(tempdir(), "no-such-dir-xyz"))
  expect_equal(nrow(res2), 0)
})

test_that("llamacpp_is_gguf handles edge cases", {
  dir <- withr::local_tempdir()
  writeBin(c(charToRaw("GGUF"), as.raw(1:10)), file.path(dir, "good.gguf"))
  writeBin(charToRaw("xx"), file.path(dir, "short.bin"))

  expect_true(ellmer:::llamacpp_is_gguf(file.path(dir, "good.gguf")))
  expect_false(ellmer:::llamacpp_is_gguf(file.path(dir, "short.bin")))
  expect_false(ellmer:::llamacpp_is_gguf(file.path(dir, "missing.gguf")))
})

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
