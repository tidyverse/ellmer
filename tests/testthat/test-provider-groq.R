test_that("can list models", {
  test_models(models_groq, has_context_window = TRUE)
})

test_that("model listing reports the context window when present", {
  local_mocked_responses(function(req) {
    response_json(
      body = list(
        data = list(
          list(
            id = "llama3-8b-8192",
            created = 1693721698,
            owned_by = "Meta",
            context_window = 8192
          ),
          list(id = "unknown", created = 1693721698, owned_by = "Groq")
        )
      )
    )
  })

  models <- models_groq(credentials = \() "")
  expect_equal(models$context_window, c(8192L, NA_integer_))
})

# Common provider interface -----------------------------------------------

test_that("defaults are reported", {
  expect_snapshot(. <- chat_groq())
})

test_that("supports tool calling", {
  chat_fun <- chat_groq
  test_tools_simple(chat_fun)
})

test_that("supports structured data", {
  # current default model does not support structured data
  chat_fun <- function(model = "openai/gpt-oss-120b", ...) {
    chat_groq(model = model, ...)
  }
  test_data_extraction(chat_fun)
})

test_that("batch chat works", {
  chat <- chat_groq(
    system_prompt = "Answer with just the city name",
    model = "llama-3.1-8b-instant",
    params = params(temperature = 0, seed = 1014),
    credentials = function() list(Authorization = "Bearer x")
  )

  prompts <- list(
    "What's the capital of Iowa?",
    "What's the capital of New York?",
    "What's the capital of California?",
    "What's the capital of Texas?"
  )

  out <- batch_chat_text(
    chat,
    prompts,
    path = test_path("batch/state-capitals-groq.json")
  )
  expect_equal(out, c("Des Moines", "Albany", "Sacramento", "Austin"))
})

test_that("batch_chat_structured works", {
  chat <- chat_groq(
    system_prompt = "Answer with just the city name",
    model = "openai/gpt-oss-20b",
    params = params(temperature = 0, seed = 1014),
    credentials = function() list(Authorization = "Bearer x")
  )

  prompts <- list(
    "What's the capital of Iowa?",
    "What's the capital of New York?",
    "What's the capital of California?",
    "What's the capital of Texas?"
  )

  out <- batch_chat_structured(
    chat,
    prompts,
    path = test_path("batch/state-capitals-groq-structured.json"),
    type = type_object(capital = type_string())
  )

  expect_s3_class(out, "data.frame")
  expect_equal(out$capital, c("Des Moines", "Albany", "Sacramento", "Austin"))
})
