# Getting started --------------------------------------------------------

test_that("can make simple request", {
  chat <- chat_ollama_test("Be as terse as possible; no punctuation")
  resp <- chat$chat("What is 1 + 1?", echo = FALSE)
  expect_match(resp, "2")
  expect_equal(chat$last_turn()@tokens > 0, c(TRUE, TRUE))
})

test_that("can make simple streaming request", {
  chat <- chat_ollama_test("Be as terse as possible; no punctuation")
  resp <- coro::collect(chat$stream("What is 1 + 1?"))
  expect_match(paste0(unlist(resp), collapse = ""), "2")
})

# Common provider interface -----------------------------------------------

test_that("can chat with tool request", {
  chat <- chat_ollama_test("Be as terse as possible; no punctuation")

  add_two_numbers <- function(x, y = 0) x + y
  chat$register_tool(
    tool(
      add_two_numbers,
      "Add two numbers together.",
      x = type_number("The first number"),
      y = type_number("The second number", required = FALSE)
    )
  )

  # Tool with no properties
  current_time <- function() Sys.time()
  chat$register_tool(tool(current_time, "Current system time"))

  # Ollama tool calling is very inconsistent, esp. with small models, so we
  # just test that the model still works when a tool call is registered.
  expect_no_error(
    coro::collect(chat$stream("What is 1 + 1?"))
  )
})

# Currently no other tests because I can't find a model that returns reliable
# results and is reasonably performant.

# Custom -----------------------------------------------------------------

test_that("as_json specialised for Ollama", {
  stub <- ProviderOllama(name = "", base_url = "", api_key = "", model = "")

  expect_snapshot(
    as_json(stub, type_object(.additional_properties = TRUE)),
    error = TRUE
  )

  obj <- type_object(
    x = type_number(required = FALSE),
    y = type_string(required = TRUE)
  )
  expect_equal(
    as_json(stub, obj),
    list(
      type = "object",
      description = "",
      properties = list(
        x = list(type = c("number"), description = ""),
        y = list(type = c("string"), description = "")
      ),
      required = list("y"),
      additionalProperties = FALSE
    )
  )
})
