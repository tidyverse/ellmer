test_record_replay <- function(x, tools = list(), .envir = parent.frame()) {
  recorded <- contents_record(x)
  replayed <- contents_replay(recorded, tools = tools, .envir = .envir)
  expect_equal(replayed, x)
}
