test_that("NULLS + values", {
  expect_equal(merge_dicts(list(), list(a = 1)), list(a = 1))
  expect_equal(merge_dicts(list(a = 1), list()), list(a = 1))
})

test_that("equal values are ok", {
  expect_equal(merge_dicts(list(a = 1), list(a = 1)), list(a = 1))
  expect_equal(merge_dicts(list(a = 1.5), list(a = 1.5)), list(a = 1.5))
  expect_equal(
    merge_dicts(list(a = list(b = 1)), list(a = list(c = 2))),
    list(a = list(b = 1, c = 2))
  )

  expect_equal(merge_dicts(list(a = TRUE), list(a = TRUE)), list(a = TRUE))
  expect_equal(merge_dicts(list(a = FALSE), list(a = FALSE)), list(a = FALSE))

  expect_equal(merge_dicts(list(a = "x"), list(a = "x")), list(a = "x"))
})

test_that("strings are concatenated", {
  expect_equal(merge_dicts(list(a = "a"), list(a = "b")), list(a = "ab"))
  expect_equal(
    merge_dicts(list(a = list(b = "a")), list(a = list(b = "b"))),
    list(a = list(b = "ab"))
  )
})

test_that("can merge dictionaries with different keys", {
  expect_equal(
    merge_dicts(list(a = 1, b = 2), list(a = 1)),
    list(a = 1, b = 2)
  )
  expect_equal(
    merge_dicts(list(a = 1, b = 2), list(c = NULL)),
    list(a = 1, b = 2, c = NULL)
  )
})

test_that("nulls don't overwrite existing values (#115)", {
  expect_equal(
    merge_dicts(list(a = 1, b = 2), list(a = NULL)),
    list(a = 1, b = 2)
  )
})

test_that("can merge lists", {
  expect_equal(
    merge_dicts(list(a = list(1, 2)), list()),
    list(a = list(1, 2))
  )
  expect_equal(
    merge_dicts(list(), list(a = list(1, 2))),
    list(a = list(1, 2))
  )
  expect_equal(
    merge_dicts(list(a = list(1, 2)), list(a = list(3))),
    list(a = list(1, 2, 3))
  )
})

test_that("respects index when merging lists", {
  expect_equal(
    merge_dicts(
      list(a = list(list(index = 0, b = "{"))),
      list(a = list(list(index = 0, b = "f")))
    ),
    list(a = list(list(index = 0, b = "{f")))
  )

  expect_equal(
    merge_dicts(
      list(a = list(list(index = 0, b = "a"))),
      list(a = list(list(index = 1, b = "b")))
    ),
    list(a = list(list(index = 0, b = "a"), list(index = 1, b = "b")))
  )
})

test_that("we can merge OpenAI's chunk format", {
  # Example from: https://platform.openai.com/docs/api-reference/chat-streaming/streaming
  messages <- list(
    '{"id":"chatcmpl-123","object":"chat.completion.chunk","created":1694268190,"model":"gpt-4o-mini", "system_fingerprint": "fp_44709d6fcb", "choices":[{"index":0,"delta":{"role":"assistant","content":""},"logprobs":null,"finish_reason":null}]}',
    '{"id":"chatcmpl-123","object":"chat.completion.chunk","created":1694268190,"model":"gpt-4o-mini", "system_fingerprint": "fp_44709d6fcb", "choices":[{"index":0,"delta":{"content":"Hello"},"logprobs":null,"finish_reason":null}]}',
    '{"id":"chatcmpl-123","object":"chat.completion.chunk","created":1694268190,"model":"gpt-4o-mini", "system_fingerprint": "fp_44709d6fcb", "choices":[{"index":0,"delta":{},"logprobs":null,"finish_reason":"stop"}]}'
  )
  chunks <- lapply(messages, jsonlite::parse_json)
  expect_equal(
    merge_dicts(merge_dicts(chunks[[1]], chunks[[2]]), chunks[[3]]),
    list(
      id = "chatcmpl-123",
      object = "chat.completion.chunk",
      created = 1694268190,
      model = "gpt-4o-mini",
      system_fingerprint = "fp_44709d6fcb",
      choices = list(list(
        index = 0,
        delta = list(
          role = "assistant",
          content = "Hello"
        ),
        logprobs = NULL,
        finish_reason = "stop"
      ))
    )
  )
})
