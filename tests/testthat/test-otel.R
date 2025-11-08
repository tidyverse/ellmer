test_that("tracing works as expected for synchronous chats", {
  skip_if_not_installed("otelsdk")

  # Capture spans from a typical synchronous chat with tool calls.
  spans <- with_otel_record({
    test_tools_simple(chat_openai_test)
  })[["traces"]]

  # Check we have two top-level "invoke_agent" spans (one for each chat()
  # invocation) that start their respective traces.
  agent_spans <- Filter(function(x) x$name == "invoke_agent", spans)
  expect_length(agent_spans, 2L)
  expect_true(all(vapply(
    agent_spans,
    function(x) identical(x$parent, "0000000000000000"),
    logical(1)
  )))
  agent_span_ids <- sapply(agent_spans, function(x) x$span_id)

  # We should have two "execute_tool" spans (one for each tool invocation)
  # that are children of the agent spans.
  tool_spans <- Filter(function(x) startsWith(x$name, "execute_tool"), spans)
  expect_length(tool_spans, 2L)
  expect_true(all(vapply(
    tool_spans,
    function(x) x$parent %in% agent_span_ids,
    logical(1)
  )))

  # And "chat" spans that correspond to model calls before and after each
  # tool call -- these are also children of the agent spans and siblings of one
  # another.
  # Note 2025/09: Not all models are calling tools the same. There must be AT LEAST 2 chat spans. Ex: anthropic has 4, openai has 3. :shrug:
  chat_spans <- Filter(function(x) startsWith(x$name, "chat"), spans)
  expect_gte(length(chat_spans), 2L)
  expect_true(all(vapply(
    chat_spans,
    function(x) x$parent %in% agent_span_ids,
    logical(1)
  )))

  # Ensure we record the result (and therefore set the status) of chat spans.
  expect_true(all(vapply(chat_spans, function(x) x$status == "ok", logical(1))))

  # We should also get some underlying HTTP spans that correspond to the model
  # calls.
  chat_span_ids <- sapply(chat_spans, function(x) x$span_id)
  http_spans <- Filter(function(x) startsWith(x$name, "POST"), spans)
  expect_true(all(vapply(
    http_spans,
    function(x) x$parent %in% chat_span_ids,
    logical(1)
  )))
})

test_that("tracing works as expected for synchronous streams", {
  skip_if_not_installed("otelsdk")

  # Capture spans when a stream is suspended.
  spans <- with_otel_record({
    chat <- chat_openai_test(system_prompt = "Be terse.")
    stream <- chat$stream("What's your favourite Apache Spark feature?")
    local(otel::start_local_active_span("simultaneous"))
    coro::collect(stream)
  })[["traces"]]

  # Check we have one top-level "invoke_agent" span.
  expect_equal(spans[["invoke_agent"]]$parent, "0000000000000000")

  # And one child span for the stream (confirming that it ends when collected).
  chat_spans <- Filter(function(x) startsWith(x$name, "chat"), spans)
  expect_length(chat_spans, 1L)
  expect_true(all(vapply(
    chat_spans,
    function(x) identical(x$parent, spans[["invoke_agent"]]$span_id),
    logical(1)
  )))

  # Verify that the span started when the stream was suspended is not part of
  # the agent trace.
  expect_equal(spans[["simultaneous"]]$parent, "0000000000000000")

  # But that HTTP spans are.
  chat_span_ids <- sapply(chat_spans, function(x) x$span_id)
  http_spans <- Filter(function(x) startsWith(x$name, "POST"), spans)
  expect_true(all(vapply(
    http_spans,
    function(x) x$parent %in% chat_span_ids,
    logical(1)
  )))
})

test_that("tracing works as expected for asynchronous chats", {
  skip_if_not_installed("otelsdk")

  # chat <- chat_openai_test(
  chat <- chat_anthropic_test(
    system_prompt = "Always use a tool to answer. Reply with 'It is ____.'."
  )
  chat$register_tool(tool(
    coro::async(function() "2024-01-01"),
    "Return the current date",
    name = "current_date"
  ))

  # Capture spans for an async chat with async tool calls interleaved with
  # other synchronous and asynchronous spans.
  spans <- with_otel_record({
    p1 <- chat$chat_async("What's the current date in Y-M-D format?") |>
      promises::then(function(result) {
        chat$chat_async("What date will it be 47 days from now?")
      })
    p2 <- promises::promise(function(resolve, reject) {
      span <- otel::start_span("concurrent")
      otel::local_active_span(span)
      later::later(
        function() {
          on.exit(span$end())
          otel::with_active_span(span, {
            resolve(NULL)
          })
        },
        0.1
      )
    })
    local(otel::start_local_active_span("simultaneous"))
    sync(p2)
    sync(p1)
  })[["traces"]]

  # Check we have two top-level "invoke_agent" spans (one for each chat()
  # invocation) that start their respective traces.
  agent_spans <- Filter(function(x) x$name == "invoke_agent", spans)
  expect_length(agent_spans, 2L)

  expect_true(all(vapply(
    agent_spans,
    function(x) identical(x$parent, "0000000000000000"),
    logical(1)
  )))
  agent_span_ids <- sapply(agent_spans, function(x) x$span_id)

  # We should have two "execute_tool" spans (one for each tool invocation)
  # that are children of the agent spans.
  tool_spans <- Filter(function(x) startsWith(x$name, "execute_tool"), spans)
  expect_gte(length(tool_spans), 1L)
  expect_true(all(vapply(
    tool_spans,
    function(x) x$parent %in% agent_span_ids,
    logical(1)
  )))

  # And four "chat" spans that correspond to model calls before and after each
  # tool call -- these are also children of the agent spans and siblings of one
  # another.
  chat_spans <- Filter(function(x) startsWith(x$name, "chat"), spans)
  expect_gte(length(chat_spans), 2L)
  expect_true(all(vapply(
    chat_spans,
    function(x) x$parent %in% agent_span_ids,
    logical(1)
  )))

  # Ensure we record the result (and therefore set the status) of chat spans.
  expect_true(all(vapply(chat_spans, function(x) x$status == "ok", logical(1))))

  # We should also get some underlying HTTP spans that correspond to the model
  # calls.
  chat_span_ids <- sapply(chat_spans, function(x) x$span_id)
  http_spans <- Filter(function(x) startsWith(x$name, "POST"), spans)
  expect_true(all(vapply(
    http_spans,
    function(x) x$parent %in% chat_span_ids,
    logical(1)
  )))
})

test_that("tracing works as expected for asynchronous streams", {
  skip_if_not_installed("otelsdk")

  # Capture spans when an async stream is used in concert with other
  # synchronous and asynchronous spans.
  spans <- with_otel_record({
    chat <- chat_openai_test(system_prompt = "Be terse.")
    stream <- chat$stream_async("What's your favourite Apache Spark feature?")
    p <- promises::promise(function(resolve, reject) {
      span <- local(otel::start_span("concurrent"))
      later::later(
        function() {
          on.exit(span$end())
          otel::with_active_span(span, {
            resolve(NULL)
          })
        },
        0.1
      )
    })
    local(otel::start_local_active_span("simultaneous"))
    sync(p)
    sync(coro::async_collect(stream))
  })[["traces"]]

  # Check we have one top-level "invoke_agent" span.
  expect_equal(spans[["invoke_agent"]]$parent, "0000000000000000")

  # And one child span for the stream (confirming that it ends when collected).
  chat_spans <- Filter(function(x) startsWith(x$name, "chat"), spans)
  expect_length(chat_spans, 1L)
  expect_true(all(vapply(
    chat_spans,
    function(x) identical(x$parent, spans[["invoke_agent"]]$span_id),
    logical(1)
  )))

  # Verify that the spans started when the stream was suspended are not part of
  # the agent trace.
  expect_equal(spans[["concurrent"]]$parent, "0000000000000000")
  expect_equal(spans[["simultaneous"]]$parent, "0000000000000000")

  # But that HTTP spans are.
  chat_span_ids <- sapply(chat_spans, function(x) x$span_id)
  http_spans <- Filter(function(x) startsWith(x$name, "POST"), spans)
  expect_true(all(vapply(
    http_spans,
    function(x) x$parent %in% chat_span_ids,
    logical(1)
  )))
})
