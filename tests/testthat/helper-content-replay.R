test_record_replay <- function(x, tools = list()) {
  recorded <- contents_record(x)
  replayed <- contents_replay(recorded, tools = tools)
  expect_equal(replayed, x)
}
