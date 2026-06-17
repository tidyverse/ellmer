test_that("can list models", {
  test_models(models_groq)
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
  chat_fun <- function(model = "openai/gpt-oss-20b", ...) {
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
    path = test_path("batch/state-capitals-groq.json"),
    ignore_hash = TRUE
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
    type = type_object(capital = type_string()),
    ignore_hash = TRUE
  )

  expect_s3_class(out, "data.frame")
  expect_equal(out$capital, c("Des Moines", "Albany", "Sacramento", "Austin"))
})
