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
local_openrouter_static_prices <- function(
  model = "openrouter/test-model",
  frame = parent.frame()
) {
  cache_path <- local_prices_cache(frame = frame)
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
  include_usage = TRUE,
  prompt_tokens = 10,
  completion_tokens = 5,
  cached_tokens = 0
) {
  if (is.null(message)) {
    message <- list(role = "assistant", content = "hi")
  }

  result <- list(
    choices = list(list(message = message, finish_reason = finish_reason))
  )
  if (include_usage) {
    usage <- list(
      prompt_tokens = prompt_tokens,
      completion_tokens = completion_tokens,
      prompt_tokens_details = list(cached_tokens = cached_tokens)
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
    resp_body_json = function(response) {
      if (inherits(response, "httr2_response")) {
        httr2::resp_body_json(response)
      } else {
        response
      }
    },
    resp_timing = function(response) c(total = 0),
    .env = env
  )
}

# Offline model catalogue --------------------------------------------------

# The first two entries are extracted from the public response of
# https://openrouter.ai/api/v1/models; the remaining entries are minimal
# edge cases: a router-priced model ("-1" sentinel), a model without a
# cache-read rate, and a free model with a null completion rate.
openrouter_models_fixture <- '{
  "data": [
    {
      "id": "minimax/minimax-m3.1-flash:free",
      "name": "MiniMax: MiniMax M3.1 Flash (free)",
      "created": 1792379569,
      "context_length": 1000000,
      "pricing": {"prompt": "0", "completion": "0"}
    },
    {
      "id": "deepseek/deepseek-v4.1-flash-1017",
      "name": "DeepSeek: DeepSeek V4.1 Flash 1017",
      "created": 1792225624,
      "context_length": 1048576,
      "pricing": {
        "prompt": "0.00000015",
        "completion": "0.0000006",
        "input_cache_read": "0.000000003"
      }
    },
    {
      "id": "openai/gpt-router-5",
      "name": "OpenAI: GPT Router 5 (dynamic pricing)",
      "created": 1791200000,
      "context_length": 400000,
      "pricing": {"prompt": "-1", "completion": "-1"}
    },
    {
      "id": "mistral/mistral-small-latest",
      "name": "Mistral: Mistral Small (latest)",
      "created": 1791300000,
      "context_length": 128000,
      "pricing": {"prompt": "0.000002", "completion": "0.000006"}
    },
    {
      "id": "meta/llama-4-scout:free",
      "name": "Meta: Llama 4 Scout (free)",
      "created": 1791400000,
      "context_length": 163840,
      "pricing": {"prompt": "0", "completion": null, "input_cache_read": "0"}
    }
  ]
}'

local_openrouter_models <- function(
  body = openrouter_models_fixture,
  frame = parent.frame()
) {
  state <- new.env(parent = emptyenv())
  state$request <- NULL

  local_mocked_bindings(
    req_perform = function(req) {
      state$request <- req
      httr2::new_response(
        method = "GET",
        url = req$url,
        status_code = 200,
        headers = list(`content-type` = "application/json"),
        body = charToRaw(body)
      )
    },
    openrouter_models_cache = openrouter_models_cache_factory(),
    .env = frame
  )

  state
}

test_that("value_turn() uses the reported cost", {
  local_openrouter_static_prices()
  # A fresh cache over an empty table keeps the missing-charge cases
  # offline and the unknown test model unknown, with no real request.
  local_openrouter_models(body = '{"data": []}')

  provider <- openrouter_test_provider()
  model <- test_model("openrouter/test-model")

  # The fixture's static prices would charge $0.02 for these tokens
  expect_equal(
    get_token_cost(
      "OpenRouter",
      "openrouter/test-model",
      tokens(input = 10, output = 5)
    ),
    dollars(0.02)
  )

  # The reported charge wins over the static estimate, including zero and
  # high-precision values.
  reported <- value_turn(provider, model, openrouter_value_result(cost = 0.005))
  expect_equal(unclass(reported@cost), 0.005)
  expect_equal(unname(reported@tokens), c(10, 5, 0))

  zero <- value_turn(provider, model, openrouter_value_result(cost = 0))
  expect_equal(unclass(zero@cost), 0)

  tiny <- value_turn(
    provider,
    model,
    openrouter_value_result(cost = 0.000123456789)
  )
  expect_equal(unclass(tiny@cost), 0.000123456789)

  # Missing or malformed charges stay unknown rather than falling back to
  # the static estimate.
  unknown <- list(
    openrouter_value_result(include_usage = FALSE),
    openrouter_value_result(include_cost = FALSE),
    openrouter_value_result(cost = NULL),
    openrouter_value_result(cost = "0.005"),
    openrouter_value_result(cost = list(0.005)),
    openrouter_value_result(cost = NaN),
    openrouter_value_result(cost = Inf)
  )
  for (result in unknown) {
    turn <- value_turn(provider, model, result)
    expect_equal(unclass(turn@cost), NA_real_)
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

test_that("value_turn() keeps reported cost through tool and structured output", {
  local_openrouter_static_prices()

  provider <- openrouter_test_provider()
  model <- test_model("openrouter/test-model")

  tool <- value_turn(
    provider,
    model,
    openrouter_value_result(
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
  )

  expect_s7_class(tool@contents[[1]], ContentToolRequest)
  expect_equal(tool@contents[[1]]@name, "current_date")
  expect_equal(tool@finish_reason, "tool_use")
  expect_equal(unclass(tool@cost), 0.0075)

  json <- value_turn(
    provider,
    model,
    openrouter_value_result(
      cost = 0.0021,
      message = list(role = "assistant", content = '{"a":1}')
    ),
    has_type = TRUE
  )

  expect_s7_class(json@contents[[1]], ContentJson)
  expect_equal(json@contents[[1]]@parsed, list(a = 1))
  expect_equal(unclass(json@cost), 0.0021)
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

test_that("stream() and stream_async() take cost from the final usage-only chunk", {
  local_openrouter_static_prices()
  local_openrouter_models()
  local_mocked_openrouter_perform(
    stream_chunks = openrouter_stream_chunks(cost = 0.0123)
  )

  chat <- chat_openrouter(model = "openrouter/test-model", echo = "none")
  out <- coro::collect(chat$stream("hi"))
  expect_equal(paste0(unlist(out), collapse = ""), "Hello world\n")

  turn <- chat$last_turn()
  expect_equal(unclass(turn@cost), 0.0123)
  expect_equal(unname(turn@tokens), c(9, 4, 3))

  local_mocked_openrouter_perform(
    stream_chunks = openrouter_stream_chunks(cost = 0.0123)
  )
  chat <- chat_openrouter(model = "openrouter/test-model", echo = "none")
  out <- sync(coro::async_collect(chat$stream_async("hi")))
  expect_equal(paste0(unlist(out), collapse = ""), "Hello world\n")

  expect_equal(unclass(chat$last_turn()@cost), 0.0123)

  # A final chunk without a reported cost is estimated from the
  # session-cached advertised rates (12 prompt tokens, 3 cached)
  local_mocked_openrouter_perform(
    stream_chunks = openrouter_stream_chunks(cost = NULL)
  )
  chat <- chat_openrouter(
    model = "deepseek/deepseek-v4.1-flash-1017",
    echo = "none"
  )
  coro::collect(chat$stream("hi"))
  expect_equal(
    unclass(chat$last_turn()@cost),
    9 * 0.15 / 1e6 + 4 * 0.6 / 1e6 + 3 * 0.003 / 1e6
  )
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
  # Keep the missing-charge case offline and unknown (empty rate table)
  local_openrouter_models(body = '{"data": []}')

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

  expect_equal(token_usage()$price, NA_real_)
  expect_equal(unclass(chat$get_cost()), NA_real_)
})

# Models ------------------------------------------------------------------

test_that("can list models", {
  vcr::local_cassette("openrouter-list-models")
  test_models(models_openrouter)
})

test_that("can list models without credentials", {
  withr::local_envvar(OPENROUTER_API_KEY = NA)
  state <- local_openrouter_models()
  # key_get() skips rather than errors when the variable is unset, so make a
  # regression to the credential path fail loudly
  local_mocked_bindings(openrouter_key = function() stop("must not be called"))

  expect_equal(nrow(models_openrouter()), 5)
  expect_equal(state$request$url, "https://openrouter.ai/api/v1/models")
  expect_null(state$request$headers$Authorization)
})

test_that("advertised prices convert to USD per million tokens", {
  local_openrouter_models()
  models <- models_openrouter()
  rates <- models[c("input", "output", "cached_input")]
  rownames(rates) <- models$id

  expect_equal(
    rates[
      c(
        "deepseek/deepseek-v4.1-flash-1017",
        "minimax/minimax-m3.1-flash:free",
        "openai/gpt-router-5",
        "meta/llama-4-scout:free"
      ),
    ],
    data.frame(
      input = c(0.15, 0, NA, 0),
      output = c(0.6, 0, NA, NA),
      cached_input = c(0.003, NA, NA, 0),
      row.names = c(
        "deepseek/deepseek-v4.1-flash-1017",
        "minimax/minimax-m3.1-flash:free",
        "openai/gpt-router-5",
        "meta/llama-4-scout:free"
      )
    )
  )
})

test_that("model listing fails normally on request errors", {
  local_mocked_bindings(
    req_perform = function(req) {
      rlang::abort("Failed to connect to OpenRouter")
    }
  )

  expect_snapshot(error = TRUE, models_openrouter())
})

# Cost fallback (#1189) ---------------------------------------------------

fallback_cost <- function(model, ...) {
  turn <- value_turn(
    openrouter_test_provider(),
    test_model(model),
    openrouter_value_result(include_cost = FALSE, ...)
  )
  unclass(turn@cost)
}

test_that("missing cost with reported counts is estimated from advertised rates", {
  local_openrouter_models()

  expect_equal(
    fallback_cost("deepseek/deepseek-v4.1-flash-1017"),
    10 * 0.15 / 1e6 + 5 * 0.6 / 1e6
  )
  expect_equal(
    fallback_cost("deepseek/deepseek-v4.1-flash-1017", cached_tokens = 3),
    7 * 0.15 / 1e6 + 5 * 0.6 / 1e6 + 3 * 0.003 / 1e6
  )
})

test_that("reported charges take precedence over estimates", {
  local_openrouter_models()
  provider <- openrouter_test_provider()
  model <- test_model("deepseek/deepseek-v4.1-flash-1017")

  zero <- value_turn(provider, model, openrouter_value_result(cost = 0))
  nonzero <- value_turn(provider, model, openrouter_value_result(cost = 0.005))

  expect_equal(unclass(zero@cost), 0)
  expect_equal(unclass(nonzero@cost), 0.005)
})

test_that("absent or incomplete usage stays unknown", {
  local_openrouter_models()
  model <- "deepseek/deepseek-v4.1-flash-1017"

  expect_equal(fallback_cost(model, include_usage = FALSE), NA_real_)
  expect_equal(fallback_cost(model, completion_tokens = NULL), NA_real_)
  expect_equal(fallback_cost(model, prompt_tokens = NULL), NA_real_)
})

test_that("costs stay unknown when the model lookup fails", {
  local_mocked_bindings(
    req_perform = function(req) rlang::abort("Failed to connect to OpenRouter"),
    openrouter_models_cache = openrouter_models_cache_factory()
  )

  expect_equal(fallback_cost("deepseek/deepseek-v4.1-flash-1017"), NA_real_)
  expect_equal(fallback_cost("deepseek/deepseek-v4.1-flash-1017"), NA_real_)
})

test_that("estimates follow advertised rate availability", {
  local_openrouter_models()

  expect_equal(fallback_cost("minimax/minimax-m3.1-flash:free"), 0)
  expect_equal(fallback_cost("openrouter/not-a-real-model"), NA_real_)
  expect_equal(fallback_cost("openai/gpt-router-5"), NA_real_)

  # A missing rate only matters for token categories that were used
  expect_equal(fallback_cost("meta/llama-4-scout:free"), NA_real_)
  expect_equal(
    fallback_cost("meta/llama-4-scout:free", completion_tokens = 0),
    0
  )
  expect_equal(
    fallback_cost("mistral/mistral-small-latest"),
    10 * 2 / 1e6 + 5 * 6 / 1e6
  )
  expect_equal(
    fallback_cost("mistral/mistral-small-latest", cached_tokens = 2),
    NA_real_
  )
})

test_that("actual charges and fallback estimates aggregate in token_usage()", {
  local_tokens()
  local_openrouter_models()

  chat <- chat_openrouter(
    model = "deepseek/deepseek-v4.1-flash-1017",
    echo = "none"
  )

  local_mocked_openrouter_perform(
    value_result = openrouter_value_result(cost = 0.01)
  )
  chat$chat("hi")

  local_mocked_openrouter_perform(
    value_result = openrouter_value_result(include_cost = FALSE)
  )
  chat$chat("hi again")

  expect_equal(token_usage()$price, 0.01 + 10 * 0.15 / 1e6 + 5 * 0.6 / 1e6)
  expect_equal(token_usage()$input, 20)
  expect_equal(token_usage()$output, 10)
})
