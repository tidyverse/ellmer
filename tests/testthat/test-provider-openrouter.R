# Getting started --------------------------------------------------------

test_that("can make simple request", {
  chat <- chat_openrouter_test("Be as terse as possible; no punctuation")
  resp <- chat$chat("What is 1 + 1?", echo = FALSE)
  expect_match(resp, "2")
  expect_equal(unname(chat$last_turn()@tokens[1:2] > 0), c(TRUE, TRUE))
})

test_that("can make simple streaming request", {
  chat <- chat_openrouter_test("Be as terse as possible; no punctuation")
  resp <- coro::collect(chat$stream("What is 1 + 1?"))
  expect_match(paste0(unlist(resp), collapse = ""), "2")
})

test_that("handles errors", {
  chat <- chat_openrouter_test(api_args = list(temperature = "hot"))
  expect_snapshot(error = TRUE, {
    chat$chat("What is 1 + 1?", echo = FALSE)
    chat$chat("What is 1 + 1?", echo = TRUE)
  })
})

# Common provider interface -----------------------------------------------

test_that("supports tool calling", {
  chat_fun <- chat_openrouter_test

  test_tools_simple(chat_fun)
})

test_that("can extract data", {
  chat_fun <- chat_openrouter_test

  test_data_extraction(chat_fun)
})

test_that("can use images", {
  chat_fun <- chat_openrouter_test

  test_images_inline(chat_fun)
  test_images_remote(chat_fun)
})

# Response-reported actual costs (#1189) ----------------------------------

openrouter_test_provider <- function() {
  ProviderOpenRouter(
    name = "OpenRouter",
    base_url = "https://openrouter.ai/api/v1",
    credentials = NULL,
    extra_headers = character(),
    preserve_thinking = TRUE,
    strict = FALSE
  )
}

# Deliberately extreme static prices for an "OpenRouter" row: any leak of
# the static estimate into a turn's cost would be unmistakable (these tokens
# would cost $0.02, not the reported charge).
local_openrouter_static_prices <- function(model = "openrouter/test-model") {
  cache_path <- local_prices_cache()
  write_prices_cache(
    cache_path,
    data.frame(
      provider = "OpenRouter",
      model = model,
      variant = "",
      input = 1000,
      output = 2000,
      cached_input = 100
    ),
    updated_at = "2126-01-01T00:00:00Z"
  )
}

openrouter_value_result <- function(
  cost = NULL,
  message = NULL,
  finish_reason = "stop",
  include_cost = TRUE,
  include_usage = TRUE
) {
  if (is.null(message)) {
    message <- list(role = "assistant", content = "hi")
  }

  result <- list(
    choices = list(list(message = message, finish_reason = finish_reason))
  )
  if (include_usage) {
    usage <- list(
      prompt_tokens = 10,
      completion_tokens = 5,
      prompt_tokens_details = list(cached_tokens = 0)
    )
    if (include_cost) {
      usage$cost <- cost
    }
    result$usage <- usage
  }
  result
}

# OpenAI-style SSE chunks, ending with the usage-only chunk (no choices)
# that OpenRouter sends last.
openrouter_stream_chunks <- function(cost = 0.0123) {
  list(
    list(
      id = "resp-test",
      choices = list(list(
        index = 0,
        delta = list(role = "assistant", content = "Hello"),
        finish_reason = NULL
      ))
    ),
    list(
      id = "resp-test",
      choices = list(list(
        index = 0,
        delta = list(content = " world"),
        finish_reason = "stop"
      ))
    ),
    list(
      id = "resp-test",
      choices = list(),
      usage = list(
        prompt_tokens = 12,
        completion_tokens = 4,
        prompt_tokens_details = list(cached_tokens = 3),
        cost = cost
      )
    )
  )
}

# Mock chat_perform so a real ProviderOpenRouter parses deterministic
# responses without touching the network.
local_mocked_openrouter_perform <- function(
  value_result = NULL,
  stream_chunks = NULL,
  env = rlang::caller_env()
) {
  local_mocked_bindings(
    chat_perform = function(provider, model, mode, ...) {
      switch(
        mode,
        value = value_result,
        stream = {
          gen <- coro::generator(function() {
            for (chunk in stream_chunks) {
              yield(chunk)
            }
          })
          gen()
        },
        "async-stream" = {
          gen <- coro::async_generator(function() {
            for (chunk in stream_chunks) {
              yield(chunk)
            }
          })
          gen()
        }
      )
    },
    # Follow the existing value-mode mock convention (see test-chat.R)
    resp_body_json = function(response) response,
    resp_timing = function(response) c(total = 0),
    .env = env
  )
}

test_that("value_turn() prefers reported cost over conflicting static prices", {
  local_openrouter_static_prices()

  provider <- openrouter_test_provider()
  model <- test_model("openrouter/test-model")

  # With the fixture's prices these tokens would cost $0.02
  turn <- value_turn(provider, model, openrouter_value_result(cost = 0.005))

  expect_equal(unclass(turn@cost), 0.005)
  expect_equal(unname(turn@tokens), c(10, 5, 0))
})

test_that("value_turn() preserves zero and high-precision costs", {
  local_openrouter_static_prices()

  provider <- openrouter_test_provider()
  model <- test_model("openrouter/test-model")

  zero <- value_turn(provider, model, openrouter_value_result(cost = 0))
  expect_equal(unclass(zero@cost), 0)
  expect_false(is.na(zero@cost))

  tiny <- value_turn(
    provider,
    model,
    openrouter_value_result(cost = 0.000123456789)
  )
  expect_equal(unclass(tiny@cost), 0.000123456789)
})

test_that("value_turn() reports unknown cost when usage or cost is missing", {
  local_openrouter_static_prices()

  provider <- openrouter_test_provider()
  model <- test_model("openrouter/test-model")

  no_usage <- value_turn(
    provider,
    model,
    openrouter_value_result(include_usage = FALSE)
  )
  no_cost <- value_turn(
    provider,
    model,
    openrouter_value_result(include_cost = FALSE)
  )
  null_cost <- value_turn(
    provider,
    model,
    openrouter_value_result(cost = NULL)
  )

  for (turn in list(no_usage, no_cost, null_cost)) {
    expect_true(is.na(turn@cost))
    expect_equal(turn@text, "hi")
  }
})

test_that("value_turn() treats invalid costs as unknown but keeps the turn", {
  local_openrouter_static_prices()

  provider <- openrouter_test_provider()
  model <- test_model("openrouter/test-model")

  invalid <- list(
    openrouter_value_result(cost = "0.005"),
    openrouter_value_result(cost = list(0.005)),
    openrouter_value_result(cost = NaN),
    openrouter_value_result(cost = Inf)
  )

  for (result in invalid) {
    turn <- value_turn(provider, model, result)
    expect_true(is.na(turn@cost))
    expect_equal(turn@text, "hi")
  }
})

test_that("value_turn() preserves error checking", {
  provider <- openrouter_test_provider()
  model <- test_model("openrouter/test-model")

  result <- list(
    error = list(
      message = "Provider returned error",
      metadata = list(raw = list(data = '{"message": "boom"}'))
    )
  )

  expect_snapshot(error = TRUE, value_turn(provider, model, result))
})

test_that("value_turn() keeps reported cost through tool-call parsing", {
  local_openrouter_static_prices()

  provider <- openrouter_test_provider()
  model <- test_model("openrouter/test-model")

  result <- openrouter_value_result(
    cost = 0.0075,
    finish_reason = "tool_calls",
    message = list(
      role = "assistant",
      content = NULL,
      tool_calls = list(list(
        id = "call_1",
        type = "function",
        `function` = list(
          name = "current_date",
          arguments = "{}"
        )
      ))
    )
  )

  turn <- value_turn(provider, model, result)

  expect_s7_class(turn@contents[[1]], ContentToolRequest)
  expect_equal(turn@contents[[1]]@name, "current_date")
  expect_equal(turn@finish_reason, "tool_use")
  expect_equal(unclass(turn@cost), 0.0075)
})

test_that("value_turn() keeps reported cost through structured-output parsing", {
  local_openrouter_static_prices()

  provider <- openrouter_test_provider()
  model <- test_model("openrouter/test-model")

  result <- openrouter_value_result(
    cost = 0.0021,
    message = list(role = "assistant", content = '{"a":1}')
  )

  turn <- value_turn(provider, model, result, has_type = TRUE)

  expect_s7_class(turn@contents[[1]], ContentJson)
  expect_equal(turn@contents[[1]]@parsed, list(a = 1))
  expect_equal(unclass(turn@cost), 0.0021)
})

test_that("final usage-only stream chunk supplies the reported cost", {
  provider <- openrouter_test_provider()
  model <- test_model("openrouter/test-model")

  result <- NULL
  for (chunk in openrouter_stream_chunks(cost = 0.0123)) {
    result <- stream_merge_chunks(provider, result, chunk)
  }

  turn <- value_turn(provider, model, result)

  expect_equal(unclass(turn@cost), 0.0123)
  expect_equal(unname(turn@tokens), c(9, 4, 3))
})

test_that("chat() reports actual charges and cost summary", {
  local_openrouter_static_prices()
  local_mocked_openrouter_perform(
    value_result = openrouter_value_result(cost = 0.005)
  )

  chat <- chat_openrouter(model = "openrouter/test-model", echo = "none")
  expect_equal(as.character(chat$chat("hi")), "hi")

  expect_equal(unclass(chat$last_turn()@cost), 0.005)
  expect_equal(unclass(chat$get_cost()), 0.005)
  expect_equal(unclass(chat$get_tokens()$cost), 0.005)
})

test_that("stream() merges final usage chunk into reported cost", {
  local_openrouter_static_prices()
  local_mocked_openrouter_perform(
    stream_chunks = openrouter_stream_chunks(cost = 0.0123)
  )

  chat <- chat_openrouter(model = "openrouter/test-model", echo = "none")
  out <- coro::collect(chat$stream("hi"))
  expect_equal(paste0(unlist(out), collapse = ""), "Hello world\n")

  turn <- chat$last_turn()
  expect_equal(unclass(turn@cost), 0.0123)
  expect_equal(unname(turn@tokens), c(9, 4, 3))
})

test_that("stream_async() merges final usage chunk into reported cost", {
  local_mocked_openrouter_perform(
    stream_chunks = openrouter_stream_chunks(cost = 0.0123)
  )

  chat <- chat_openrouter(model = "openrouter/test-model", echo = "none")
  out <- sync(coro::async_collect(chat$stream_async("hi")))
  expect_equal(paste0(unlist(out), collapse = ""), "Hello world\n")

  expect_equal(unclass(chat$last_turn()@cost), 0.0123)
})

test_that("chat_structured() reports actual charges", {
  local_openrouter_static_prices()
  local_mocked_openrouter_perform(
    value_result = openrouter_value_result(
      cost = 0.0021,
      message = list(role = "assistant", content = '{"a":1}')
    )
  )

  chat <- chat_openrouter(model = "openrouter/test-model", echo = "none")
  data <- chat$chat_structured("hi", type = type_object(a = type_integer()))

  expect_equal(data$a, 1)
  expect_equal(unclass(chat$last_turn()@cost), 0.0021)
})

test_that("completed turns log actual charges for token_usage()", {
  local_tokens()
  local_openrouter_static_prices()

  chat <- chat_openrouter(model = "openrouter/test-model", echo = "none")

  local_mocked_openrouter_perform(
    value_result = openrouter_value_result(cost = 0.01)
  )
  chat$chat("hi")

  local_mocked_openrouter_perform(
    value_result = openrouter_value_result(cost = 0.02)
  )
  chat$chat("hi again")

  usage <- token_usage()
  expect_equal(usage$price, 0.03)
  expect_equal(usage$input, 20)
  expect_equal(usage$output, 10)

  # Unknown charges propagate NA rather than being treated as zero
  local_mocked_openrouter_perform(
    value_result = openrouter_value_result(cost = NULL)
  )
  chat$chat("and again")

  expect_true(is.na(token_usage()$price))
  expect_true(is.na(chat$get_cost()))
})
