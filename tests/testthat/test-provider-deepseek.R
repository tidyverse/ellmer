# Getting started --------------------------------------------------------

test_that("can make simple request", {
  chat <- chat_deepseek("Be as terse as possible; no punctuation")
  resp <- chat$chat("What is 1 + 1?", echo = FALSE)
  expect_match(resp, "2")
  expect_equal(unname(chat$last_turn()@tokens[1:2] > 0), c(TRUE, TRUE))
})

test_that("can make simple streaming request", {
  chat <- chat_deepseek("Be as terse as possible; no punctuation")
  resp <- coro::collect(chat$stream("What is 1 + 1?"))
  expect_match(paste0(unlist(resp), collapse = ""), "2")
})

test_that("can list models", {
  test_models(models_deepseek)
})

# Common provider interface -----------------------------------------------

test_that("defaults are reported", {
  expect_snapshot(. <- chat_deepseek())
})

test_that("supports tool calling", {
  chat_fun <- chat_deepseek

  test_tools_simple(chat_fun)
})

test_that("reasoning effort reaches streaming and non-streaming requests", {
  for (effort in c("none", "low", "high", "max")) {
    chat <- chat_deepseek(
      model = "deepseek-flash",
      credentials = function() "test-key",
      params = params(reasoning_effort = effort)
    )
    for (stream in c(FALSE, TRUE)) {
      req <- chat_request(
        chat$get_provider(),
        chat$get_model_object(),
        stream = stream,
        turns = list(UserTurn("Hello"))
      )
      expect_equal(req_get_body(req)$reasoning_effort, effort)
      expect_null(req_get_body(req)$thinking)
    }
  }
})

test_that("reasoning effort keeps API defaults and api_args overrides", {
  chat <- chat_deepseek(
    model = "deepseek-flash",
    credentials = function() "test-key"
  )
  req <- chat_request(chat$get_provider(), chat$get_model_object())
  expect_null(req_get_body(req)$reasoning_effort)
  expect_null(req_get_body(req)$thinking)

  chat <- chat_deepseek(
    model = "deepseek-flash",
    credentials = function() "test-key",
    params = params(reasoning_effort = "high"),
    api_args = list(
      reasoning_effort = "max",
      thinking = list(type = "disabled")
    )
  )
  req <- chat_request(chat$get_provider(), chat$get_model_object())
  expect_equal(req_get_body(req)$reasoning_effort, "max")
  expect_equal(req_get_body(req)$thinking, list(type = "disabled"))
})

test_that("thinking survives tool follow-ups and subsequent user turns", {
  run_case <- function(stream) {
    requests <- list()
    messages <- list(
      list(
        role = "assistant",
        content = NULL,
        reasoning_content = "Use the calculator.",
        tool_calls = list(list(
          index = 0,
          id = "call_1",
          type = "function",
          `function` = list(name = "calculator", arguments = "{}")
        ))
      ),
      list(
        role = "assistant",
        content = "2",
        reasoning_content = "The tool returned two."
      ),
      list(role = "assistant", content = "3", reasoning_content = "Add one.")
    )
    respond <- function(req) {
      requests[[length(requests) + 1L]] <<- req_get_body(req)
      message <- messages[[length(requests)]]
      list(
        choices = list(list(
          message = message,
          finish_reason = if (is.null(message$tool_calls)) {
            "stop"
          } else {
            "tool_calls"
          }
        ))
      )
    }
    local_mocked_bindings(
      req_perform = function(req) {
        httr2::new_response(
          method = "POST",
          url = req$url,
          status_code = 200,
          headers = list("content-type" = "application/json"),
          body = charToRaw(jsonlite::toJSON(
            respond(req),
            auto_unbox = TRUE,
            null = "null"
          ))
        )
      },
      chat_perform_stream = coro::generator(function(provider, req, ...) {
        response <- respond(req)
        message <- response$choices[[1]]$message
        yield(list(
          choices = list(list(
            index = 0,
            delta = list(reasoning_content = message$reasoning_content)
          ))
        ))
        message$reasoning_content <- NULL
        yield(list(
          choices = list(list(
            index = 0,
            delta = message,
            finish_reason = response$choices[[1]]$finish_reason
          ))
        ))
      })
    )

    chat <- chat_deepseek(
      model = "deepseek-flash",
      credentials = function() "test-key",
      params = params(reasoning_effort = "max")
    )
    chat$register_tool(tool(
      function() "2",
      "Add one and one",
      name = "calculator"
    ))
    for (prompt in c("What is 1 + 1?", "Add one.")) {
      if (stream) {
        coro::collect(chat$stream(prompt))
      } else {
        chat$chat(prompt, echo = "none")
      }
    }
    expect_equal(chat$last_turn()@text, "3")

    expect_length(requests, 3)
    expect_equal(
      requests[[2]]$messages[[2]]$reasoning_content,
      "Use the calculator."
    )
    expect_equal(requests[[2]]$messages[[2]]$tool_calls[[1]]$id, "call_1")
    expect_equal(
      requests[[2]]$messages[[3]],
      list(
        role = "tool",
        content = "2",
        tool_call_id = "call_1"
      )
    )
    expect_equal(
      requests[[3]]$messages[[2]]$reasoning_content,
      "Use the calculator."
    )
    expect_equal(
      requests[[3]]$messages[[4]]$reasoning_content,
      "The tool returned two."
    )
    expect_equal(requests[[3]]$messages[[4]]$content, "2")
  }

  run_case(FALSE)
  run_case(TRUE)
})

test_that("assistant history handles multiple or absent thinking blocks", {
  provider <- chat_deepseek(model = "deepseek-flash")$get_provider()
  turn <- AssistantTurn(list(
    ContentThinking("First. "),
    ContentThinking("Second."),
    ContentText("Answer")
  ))
  expect_equal(
    as_json(provider, turn),
    list(list(
      role = "assistant",
      content = "Answer",
      reasoning_content = "First. Second."
    ))
  )
  expect_equal(
    as_json(provider, AssistantTurn("Answer")),
    list(list(
      role = "assistant",
      content = "Answer"
    ))
  )
})

# # Doesn't support data extraction
# test_that("can extract data", {
#   chat_fun <- chat_deepseek

#   test_data_extraction(chat_fun)
# })

# # Doesn't support images
# test_that("can use images", {
#   chat_fun <- chat_deepseek_test

#   test_images_inline(chat_fun)
#   test_images_remote(chat_fun)
# })
