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

# Missing-charge cases from before the fallback existed: keep them offline
# with a fresh cache over an empty table, so the unknown test model stays
# unknown without any real request.
local_openrouter_no_fallback_models <- function(frame = parent.frame()) {
  local_mocked_bindings(
    openrouter_models_cache = openrouter_models_cache_factory(),
    models_openrouter = function(...) {
      data.frame(
        id = character(),
        created_at = as.Date(.POSIXct(numeric())),
        name = character(),
        context_length = integer(),
        cached_input = numeric(),
        input = numeric(),
        output = numeric()
      )
    },
    .env = frame
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
    resp_body_json = function(response) response,
    resp_timing = function(response) c(total = 0),
    .env = env
  )
}

test_that("value_turn() prefers reported cost over conflicting static prices", {
  local_openrouter_static_prices()

  expect_equal(
    get_token_cost(
      "OpenRouter",
      "openrouter/test-model",
      tokens(input = 10, output = 5)
    ),
    dollars(0.02)
  )

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

  tiny <- value_turn(
    provider,
    model,
    openrouter_value_result(cost = 0.000123456789)
  )
  expect_equal(unclass(tiny@cost), 0.000123456789)
})

test_that("value_turn() reports unknown cost when usage or cost is missing", {
  local_openrouter_static_prices()
  local_openrouter_no_fallback_models()

  expect_equal(
    get_token_cost(
      "OpenRouter",
      "openrouter/test-model",
      tokens(input = 10, output = 5)
    ),
    dollars(0.02)
  )

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
    expect_equal(unclass(turn@cost), NA_real_)
    expect_equal(turn@text, "hi")
  }
})

test_that("value_turn() treats invalid costs as unknown but keeps the turn", {
  local_openrouter_static_prices()
  local_openrouter_no_fallback_models()

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
  local_openrouter_no_fallback_models()

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
    .env = frame
  )

  state
}

test_that("can list models without credentials", {
  withr::local_envvar(OPENROUTER_API_KEY = NA)
  state <- local_openrouter_models()
  models <- models_openrouter()

  expect_s3_class(models, "data.frame")
  expect_equal(nrow(models), 5)
  expect_equal(
    names(models),
    c(
      "id",
      "created_at",
      "name",
      "context_length",
      "cached_input",
      "input",
      "output"
    )
  )

  # No `owned_by` is invented and model ids keep their full OpenRouter form
  expect_setequal(
    models$id,
    c(
      "minimax/minimax-m3.1-flash:free",
      "deepseek/deepseek-v4.1-flash-1017",
      "openai/gpt-router-5",
      "mistral/mistral-small-latest",
      "meta/llama-4-scout:free"
    )
  )

  # The public endpoint is used, without an Authorization header
  expect_equal(state$request$url, "https://openrouter.ai/api/v1/models")
  expect_null(state$request$headers$Authorization)
  expect_contains(names(state$request$headers), c("HTTP-Referer", "X-Title"))
})

test_that("model listing never looks up OPENROUTER_API_KEY", {
  withr::local_envvar(OPENROUTER_API_KEY = NA)
  state <- local_openrouter_models()
  local_mocked_bindings(
    openrouter_key = function() stop("must not be called"),
    .env = environment()
  )

  expect_equal(nrow(models_openrouter()), 5)
})

test_that("unix timestamps become dates, newest first", {
  state <- local_openrouter_models()
  models <- models_openrouter()

  expect_s3_class(models$created_at, "Date")
  expect_equal(
    models$id,
    c(
      "minimax/minimax-m3.1-flash:free",
      "deepseek/deepseek-v4.1-flash-1017",
      "meta/llama-4-scout:free",
      "mistral/mistral-small-latest",
      "openai/gpt-router-5"
    )
  )
  expect_equal(
    models$created_at,
    as.Date(c(
      "2026-10-19",
      "2026-10-17",
      "2026-10-07",
      "2026-10-06",
      "2026-10-05"
    ))
  )
})

test_that("model metadata uses expected types", {
  state <- local_openrouter_models()
  models <- models_openrouter()

  expect_type(models$id, "character")
  expect_type(models$name, "character")
  expect_type(models$context_length, "integer")
  expect_type(models$cached_input, "double")
  expect_type(models$input, "double")
  expect_type(models$output, "double")

  deepseek <- models[models$id == "deepseek/deepseek-v4.1-flash-1017", ]
  expect_equal(deepseek$name, "DeepSeek: DeepSeek V4.1 Flash 1017")
  expect_equal(deepseek$context_length, 1048576L)
})

test_that("advertised prices convert to USD per million tokens", {
  state <- local_openrouter_models()
  models <- models_openrouter()
  row <- function(id) models[models$id == id, ]

  paid <- row("deepseek/deepseek-v4.1-flash-1017")
  expect_equal(paid$cached_input, 0.003)
  expect_equal(paid$input, 0.15)
  expect_equal(paid$output, 0.6)

  # Free models keep their genuine zero rates
  free <- row("minimax/minimax-m3.1-flash:free")
  expect_equal(free$input, 0)
  expect_equal(free$output, 0)
  # An absent cache-read rate is unknown, not zero
  expect_equal(free$cached_input, NA_real_)

  # Router-priced models use "-1" as a sentinel, not a negative price
  router <- row("openai/gpt-router-5")
  expect_equal(router$cached_input, NA_real_)
  expect_equal(router$input, NA_real_)
  expect_equal(router$output, NA_real_)

  nocache <- row("mistral/mistral-small-latest")
  expect_equal(nocache$cached_input, NA_real_)
  expect_equal(nocache$input, 2)
  expect_equal(nocache$output, 6)

  # A null rate is unknown, while other zero rates stay zero
  null_rate <- row("meta/llama-4-scout:free")
  expect_equal(null_rate$input, 0)
  expect_equal(null_rate$output, NA_real_)
  expect_equal(null_rate$cached_input, 0)
})

test_that("empty model lists return a typed zero-row data frame", {
  state <- local_openrouter_models(body = '{"data": []}')
  models <- models_openrouter()
  expect_equal(nrow(models), 0)
  expect_type(models$id, "character")
  expect_s3_class(models$created_at, "Date")
  expect_type(models$context_length, "integer")
  expect_type(models$input, "double")

  # A missing data field is treated the same as an empty list
  state <- local_openrouter_models(body = '{"total_count": 0}')
  expect_equal(nrow(models_openrouter()), 0)
})

test_that("model prices come from the API, not the local price snapshot", {
  local_prices()
  the$prices <- data.frame(
    provider = "OpenRouter",
    model = "deepseek/deepseek-v4.1-flash-1017",
    cached_input = 999,
    input = 888,
    output = 777
  )
  state <- local_openrouter_models()
  models <- models_openrouter()

  paid <- models[models$id == "deepseek/deepseek-v4.1-flash-1017", ]
  expect_equal(paid$cached_input, 0.003)
  expect_equal(paid$input, 0.15)
  expect_equal(paid$output, 0.6)
})

test_that("models_openrouter() honours base_url", {
  state <- local_openrouter_models()
  models_openrouter(base_url = "https://example.com/api/v1")
  expect_equal(state$request$url, "https://example.com/api/v1/models")
})

test_that("model listing fails normally on request errors", {
  local_mocked_bindings(
    req_perform = function(req) {
      rlang::abort("Failed to connect to OpenRouter")
    }
  )

  expect_snapshot(error = TRUE, models_openrouter())
})

test_that("openrouter_rate() guards invalid rates", {
  expect_equal(openrouter_rate("0"), 0)
  expect_equal(openrouter_rate("0.00000015"), 0.15)
  expect_equal(openrouter_rate("Inf"), NA_real_)
  expect_equal(openrouter_rate("-Inf"), NA_real_)
  expect_equal(openrouter_rate("not-a-price"), NA_real_)
  expect_equal(openrouter_rate(NULL), NA_real_)
  expect_equal(openrouter_rate(list("0")), NA_real_)
  expect_equal(openrouter_rate(c("0", "0")), NA_real_)
})

test_that("can list models", {
  vcr::local_cassette("openrouter-list-models")

  test_models(models_openrouter)
})

test_that("chat_openrouter() still looks up credentials normally", {
  withr::local_envvar(OPENROUTER_API_KEY = "dummy-not-a-real-key")
  chat <- chat_openrouter()
  expect_equal(chat$get_provider()@credentials(), "dummy-not-a-real-key")
})

# Session-cached cost fallback (#1189) ------------------------------------

# Fresh fallback state for a test: a cache closure from the same factory
# as the production one, with each table load counted at the
# models_openrouter() boundary. The req_perform() mock keeps everything
# offline and the counted wrapper proves one table serves all models.
local_openrouter_fallback_cache <- function(
  body = openrouter_models_fixture,
  frame = parent.frame()
) {
  state <- local_openrouter_models(body = body, frame = frame)
  state$fetches <- 0L

  # Parse the fixture once through the real models_list() path so tests
  # can freely combine this helper with local_mocked_openrouter_perform()
  # (whose resp_body_json() passthrough would otherwise break the parse).
  table <- models_openrouter()
  state$fetches <- 0L

  local_mocked_bindings(
    openrouter_models_cache = openrouter_models_cache_factory(),
    models_openrouter = function(...) {
      state$fetches <- state$fetches + 1L
      table
    },
    .env = frame
  )

  state
}

test_that("missing cost with reported counts is estimated from advertised rates", {
  state <- local_openrouter_fallback_cache()

  provider <- openrouter_test_provider()
  model <- test_model("deepseek/deepseek-v4.1-flash-1017")

  turn <- value_turn(
    provider,
    model,
    openrouter_value_result(include_cost = FALSE)
  )

  expect_equal(unclass(turn@cost), 10 * 0.15 / 1e6 + 5 * 0.6 / 1e6)
  expect_equal(state$fetches, 1L)
})

test_that("reported charges never trigger a model lookup", {
  state <- local_openrouter_fallback_cache()

  provider <- openrouter_test_provider()
  model <- test_model("deepseek/deepseek-v4.1-flash-1017")

  zero <- value_turn(provider, model, openrouter_value_result(cost = 0))
  nonzero <- value_turn(provider, model, openrouter_value_result(cost = 0.005))

  expect_equal(unclass(zero@cost), 0)
  expect_equal(unclass(nonzero@cost), 0.005)
  expect_equal(state$fetches, 0L)
})

test_that("absent or incomplete usage stays unknown without a model lookup", {
  state <- local_openrouter_fallback_cache()

  provider <- openrouter_test_provider()
  model <- test_model("deepseek/deepseek-v4.1-flash-1017")

  no_usage <- value_turn(
    provider,
    model,
    openrouter_value_result(include_usage = FALSE)
  )
  no_completion <- value_turn(
    provider,
    model,
    openrouter_value_result(include_cost = FALSE, completion_tokens = NULL)
  )
  no_prompt <- value_turn(
    provider,
    model,
    openrouter_value_result(include_cost = FALSE, prompt_tokens = NULL)
  )

  for (turn in list(no_usage, no_completion, no_prompt)) {
    expect_equal(unclass(turn@cost), NA_real_)
  }
  expect_equal(state$fetches, 0L)
})

test_that("one session table serves repeated turns, models, and unknowns", {
  state <- local_openrouter_fallback_cache()

  provider <- openrouter_test_provider()
  deepseek <- test_model("deepseek/deepseek-v4.1-flash-1017")
  mistral <- test_model("mistral/mistral-small-latest")

  first <- value_turn(
    provider,
    deepseek,
    openrouter_value_result(include_cost = FALSE)
  )
  again <- value_turn(
    provider,
    deepseek,
    openrouter_value_result(include_cost = FALSE)
  )
  other <- value_turn(
    provider,
    mistral,
    openrouter_value_result(include_cost = FALSE, prompt_tokens = 8)
  )

  expect_equal(unclass(first@cost), 10 * 0.15 / 1e6 + 5 * 0.6 / 1e6)
  expect_equal(unclass(again@cost), 10 * 0.15 / 1e6 + 5 * 0.6 / 1e6)
  expect_equal(unclass(other@cost), 8 * 2 / 1e6 + 5 * 6 / 1e6)
  expect_equal(state$fetches, 1L)

  # A model missing from the shared table doesn't trigger a new fetch
  unknown <- value_turn(
    provider,
    test_model("openrouter/not-a-real-model"),
    openrouter_value_result(include_cost = FALSE)
  )
  expect_equal(unclass(unknown@cost), NA_real_)
  expect_equal(state$fetches, 1L)
})

test_that("a failed table fetch is latched and attempted only once", {
  state <- new.env(parent = emptyenv())
  state$fetches <- 0L

  local_mocked_bindings(
    openrouter_models_cache = openrouter_models_cache_factory(),
    models_openrouter = function(...) {
      state$fetches <- state$fetches + 1L
      rlang::abort("Failed to connect to OpenRouter")
    }
  )

  provider <- openrouter_test_provider()
  model <- test_model("deepseek/deepseek-v4.1-flash-1017")

  first <- value_turn(
    provider,
    model,
    openrouter_value_result(include_cost = FALSE)
  )
  second <- value_turn(
    provider,
    model,
    openrouter_value_result(include_cost = FALSE)
  )

  expect_equal(unclass(first@cost), NA_real_)
  expect_equal(unclass(second@cost), NA_real_)
  expect_equal(state$fetches, 1L)
})

test_that("fallback lookup is bounded by scoped timeout and retry options", {
  observed <- new.env(parent = emptyenv())
  local_mocked_bindings(
    openrouter_models_cache = openrouter_models_cache_factory(),
    models_openrouter = function(...) {
      observed$timeout <- getOption("ellmer_timeout_s")
      observed$tries <- getOption("ellmer_max_tries")
      rlang::abort("Failed to connect to OpenRouter")
    }
  )

  withr::local_options(
    ellmer_timeout_s = 600,
    ellmer_max_tries = 5
  )

  provider <- openrouter_test_provider()
  model <- test_model("deepseek/deepseek-v4.1-flash-1017")
  turn <- value_turn(
    provider,
    model,
    openrouter_value_result(include_cost = FALSE)
  )
  expect_equal(unclass(turn@cost), NA_real_)

  # Inherited long/retrying transport is capped inside the fallback...
  expect_equal(observed$timeout, 5)
  expect_equal(observed$tries, 1)

  # ...and caller settings are restored afterwards
  expect_equal(getOption("ellmer_timeout_s"), 600)
  expect_equal(getOption("ellmer_max_tries"), 5)
})

test_that("a shorter caller timeout stays shorter inside the fallback", {
  observed <- new.env(parent = emptyenv())
  local_mocked_bindings(
    openrouter_models_cache = openrouter_models_cache_factory(),
    models_openrouter = function(...) {
      observed$timeout <- getOption("ellmer_timeout_s")
      rlang::abort("Failed to connect to OpenRouter")
    }
  )

  withr::local_options(
    ellmer_timeout_s = 2,
    ellmer_max_tries = 5
  )

  provider <- openrouter_test_provider()
  turn <- value_turn(
    provider,
    test_model("deepseek/deepseek-v4.1-flash-1017"),
    openrouter_value_result(include_cost = FALSE)
  )
  expect_equal(unclass(turn@cost), NA_real_)

  expect_equal(observed$timeout, 2)
  expect_equal(getOption("ellmer_timeout_s"), 2)
  expect_equal(getOption("ellmer_max_tries"), 5)
})

test_that("cached lookups don't reset options and direct calls see caller values", {
  observed <- new.env(parent = emptyenv())
  observed$fetches <- 0L
  state <- local_openrouter_models()
  table <- models_openrouter()
  local_mocked_bindings(
    openrouter_models_cache = openrouter_models_cache_factory(),
    models_openrouter = function(...) {
      observed$fetches <- observed$fetches + 1L
      observed$timeout <- getOption("ellmer_timeout_s")
      observed$tries <- getOption("ellmer_max_tries")
      table
    }
  )

  withr::local_options(ellmer_timeout_s = 600, ellmer_max_tries = 5)

  # Direct discovery observes the caller's unbounded settings
  expect_equal(nrow(models_openrouter()), 5)
  expect_equal(observed$timeout, 600)
  expect_equal(observed$tries, 5)

  provider <- openrouter_test_provider()
  model <- test_model("deepseek/deepseek-v4.1-flash-1017")
  args <- openrouter_value_result(include_cost = FALSE)

  # First (latching) fallback lookup sees the bounded options...
  first <- value_turn(provider, model, args)
  expect_equal(unclass(first@cost), 10 * 0.15 / 1e6 + 5 * 0.6 / 1e6)
  expect_equal(observed$fetches, 2L)
  expect_equal(observed$timeout, 5)
  expect_equal(observed$tries, 1)

  # ...later cached turns don't re-enter the lookup or touch options
  value_turn(provider, model, args)
  value_turn(provider, test_model("mistral/mistral-small-latest"), args)
  expect_equal(observed$fetches, 2L)
  expect_equal(getOption("ellmer_timeout_s"), 600)
  expect_equal(getOption("ellmer_max_tries"), 5)
})

test_that("estimates follow advertised rate availability", {
  state <- local_openrouter_fallback_cache()

  provider <- openrouter_test_provider()

  # Genuinely free models estimate to zero
  free <- value_turn(
    provider,
    test_model("minimax/minimax-m3.1-flash:free"),
    openrouter_value_result(include_cost = FALSE)
  )
  expect_equal(unclass(free@cost), 0)

  # A missing rate for a used token category can't be estimated...
  used_null_output <- value_turn(
    provider,
    test_model("meta/llama-4-scout:free"),
    openrouter_value_result(include_cost = FALSE)
  )
  expect_equal(unclass(used_null_output@cost), NA_real_)

  # ...but the same model with zero output tokens estimates fine
  zero_output <- value_turn(
    provider,
    test_model("meta/llama-4-scout:free"),
    openrouter_value_result(include_cost = FALSE, completion_tokens = 0)
  )
  expect_equal(unclass(zero_output@cost), 0)

  # Zero cached tokens don't need an advertised cache rate...
  no_cached_tokens <- value_turn(
    provider,
    test_model("mistral/mistral-small-latest"),
    openrouter_value_result(include_cost = FALSE)
  )
  expect_equal(unclass(no_cached_tokens@cost), 10 * 2 / 1e6 + 5 * 6 / 1e6)

  # ...but cached tokens without a rate do
  unknown_cache_rate <- value_turn(
    provider,
    test_model("mistral/mistral-small-latest"),
    openrouter_value_result(include_cost = FALSE, cached_tokens = 2)
  )
  expect_equal(unclass(unknown_cache_rate@cost), NA_real_)

  # Cached input uses the advertised cache-read rate
  cached <- value_turn(
    provider,
    test_model("deepseek/deepseek-v4.1-flash-1017"),
    openrouter_value_result(include_cost = FALSE, cached_tokens = 3)
  )
  expect_equal(
    unclass(cached@cost),
    7 * 0.15 / 1e6 + 5 * 0.6 / 1e6 + 3 * 0.003 / 1e6
  )

  # Router-priced models can't be estimated
  router <- value_turn(
    provider,
    test_model("openai/gpt-router-5"),
    openrouter_value_result(include_cost = FALSE)
  )
  expect_equal(unclass(router@cost), NA_real_)

  expect_equal(state$fetches, 1L)
})

test_that("explicit models_openrouter() stays fresh and never touches the cache", {
  state <- local_openrouter_fallback_cache()

  provider <- openrouter_test_provider()
  model <- test_model("deepseek/deepseek-v4.1-flash-1017")

  # Explicit discovery hits the API each time without warming the cache
  expect_equal(nrow(models_openrouter()), 5)
  expect_equal(nrow(models_openrouter()), 5)
  expect_equal(state$fetches, 2L)

  # The fallback still fetches its own session table...
  turn <- value_turn(
    provider,
    model,
    openrouter_value_result(include_cost = FALSE)
  )
  expect_equal(unclass(turn@cost), 10 * 0.15 / 1e6 + 5 * 0.6 / 1e6)
  expect_equal(state$fetches, 3L)

  # ...and explicit calls never mutate the cached table
  expect_equal(nrow(models_openrouter()), 5)
  again <- value_turn(
    provider,
    model,
    openrouter_value_result(include_cost = FALSE)
  )
  expect_equal(unclass(again@cost), 10 * 0.15 / 1e6 + 5 * 0.6 / 1e6)
  expect_equal(state$fetches, 4L)
})

test_that("actual charges and fallback estimates aggregate in token_usage()", {
  local_tokens()
  # Conflicting static prices must never serve as the fallback
  local_openrouter_static_prices(model = "deepseek/deepseek-v4.1-flash-1017")
  expect_equal(
    get_token_cost(
      "OpenRouter",
      "deepseek/deepseek-v4.1-flash-1017",
      tokens(input = 10, output = 5)
    ),
    dollars(0.02)
  )
  state <- local_openrouter_fallback_cache()

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
  expect_equal(state$fetches, 1L)
})
