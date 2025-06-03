retry_test <- function(code, retries = 1) {
  code <- enquo(code)

  i <- 1
  while (i <= retries) {
    tryCatch(
      {
        return(eval(get_expr(code), get_env(code)))
        break
      },
      expectation_failure = function(cnd) NULL
    )
    cli::cli_inform(c(i = "Retry {i}"))
    i <- i + 1
  }

  eval(get_expr(code), get_env(code))
}

# Params -----------------------------------------------------------------

test_params_stop <- function(chat_fun) {
  chat <- chat_fun(params = params(stop_sequences = "cool"))
  out <- chat$chat("Repeat after the following phrase: Dogs are cool")
  expect_equal(out, "Dogs are ")
}

# Tool calls -------------------------------------------------------------

test_tools_simple <- function(chat_fun) {
  chat <- chat_fun(
    system_prompt = "Always use a tool to answer. Reply with 'It is ____.'."
  )
  chat$register_tool(tool(
    function() "2024-01-01",
    "Return the current date",
    .name = "current_date"
  ))
  chat$register_tool(tool(
    function() "February",
    "Return the full name of the current month",
    .name = "current_month"
  ))

  result <- chat$chat("What's the current date in Y-M-D format?")
  expect_match(result, "2024-01-01")

  result <- chat$chat("What month is it? Provide the full name")
  expect_match(result, "February")
}

test_tools_async <- function(chat_fun) {
  chat <- chat_fun(system_prompt = "Be very terse, not even punctuation.")
  chat$register_tool(tool(
    coro::async(function() "2024-01-01"),
    "Return the current date"
  ))

  result <- sync(chat$chat_async("What's the current date in Y-M-D format?"))
  expect_match(result, "2024-01-01")

  expect_snapshot(
    chat$chat("Great. Do it again."),
    error = TRUE,
    transform = function(value) gsub(" \\(\\w+_[a-z0-9A-Z]+\\)", " (ID)", value)
  )
}

test_tools_parallel <- function(chat_fun, total_calls = 4) {
  chat <- chat_fun(system_prompt = "Be very terse, not even punctuation.")
  favourite_color <- function(person) {
    if (person == "Joe") "sage green" else "red"
  }
  chat$register_tool(tool(
    favourite_color,
    "Returns a person's favourite colour",
    person = type_string("Name of a person")
  ))

  result <- chat$chat(
    "
    What are Joe and Hadley's favourite colours?
    Answer like name1: colour1, name2: colour2
  "
  )
  expect_match(result, "Joe: sage green")
  expect_match(result, "Hadley: red")
  expect_length(chat$get_turns(), total_calls)
}

test_tools_sequential <- function(chat_fun, total_calls) {
  chat <- chat_fun(
    system_prompt = "
    Use provided tool calls to find the weather forecast and suitable
    equipment for a variety of weather conditions.

    In your response, be very terse and omit punctuation.
  "
  )

  forecast <- function(city) if (city == "New York") "rainy" else "sunny"
  equipment <- function(weather) {
    if (weather == "rainy") "umbrella" else "sunscreen"
  }
  chat$register_tool(tool(
    forecast,
    "Gets the weather forecast for a city",
    city = type_string("City name")
  ))
  chat$register_tool(tool(
    equipment,
    "Gets the equipment needed for a weather condition",
    weather = type_string("Weather condition")
  ))

  result <- chat$chat("What should I pack for New York this weekend?")
  expect_match(result, "umbrella", ignore.case = TRUE)
  expect_length(chat$get_turns(), total_calls)
}

# Data extraction --------------------------------------------------------

test_data_extraction <- function(chat_fun) {
  article_summary <- type_object(
    "Summary of the article. Preserve existing case.",
    title = type_string("Content title"),
    author = type_string("Name of the author")
  )

  prompt <- "
    # Apples are tasty
    By Hadley Wickham

    Apples are delicious and tasty and I like to eat them.
    Except for red delicious, that is. They are NOT delicious.
  "

  chat <- chat_fun()
  data <- chat$chat_structured(prompt, type = article_summary)
  expect_mapequal(
    data,
    list(title = "Apples are tasty", author = "Hadley Wickham")
  )

  # Check that we can do it again
  data <- chat$chat_structured(prompt, type = article_summary)
  expect_mapequal(
    data,
    list(title = "Apples are tasty", author = "Hadley Wickham")
  )
}

# Images -----------------------------------------------------------------

test_images_inline <- function(chat_fun, test_shape = TRUE) {
  chat <- chat_fun()
  response <- chat$chat(
    "What's in this image? (Be sure to mention the outside shape)",
    content_image_file(system.file("httr2.png", package = "ellmer"))
  )
  if (test_shape) {
    expect_match(response, "hex")
  }
  expect_match(response, "baseball")
}

test_images_remote <- function(chat_fun, test_shape = TRUE) {
  chat <- chat_fun()
  response <- chat$chat(
    "What's in this image? (Be sure to mention the outside shape)",
    content_image_url("https://httr2.r-lib.org/logo.png")
  )
  if (test_shape) {
    expect_match(response, "hex")
  }
  expect_match(response, "baseball")
}

test_images_remote_error <- function(chat_fun) {
  chat <- chat_fun()

  image_remote <- content_image_url("https://httr2.r-lib.org/logo.png")
  expect_snapshot(
    . <- chat$chat("What's in this image?", image_remote),
    error = TRUE
  )
  expect_length(chat$get_turns(), 0)
}

# PDF ---------------------------------------------------------------------

test_pdf_local <- function(chat_fun) {
  chat <- chat_fun()
  response <- chat$chat(
    "What's the title of this document?",
    content_pdf_file(test_path("apples.pdf"))
  )
  expect_match(response, "Apples are tasty")
  expect_match(chat$chat("What apple is not tasty?"), "red delicious")
}

# Models ------------------------------------------------------------------

test_models <- function(models_fun) {
  models <- models_fun()
  expect_gt(nrow(models), 0)
  expect_s3_class(models, "data.frame")
  expect_contains(names(models), "id")
}
