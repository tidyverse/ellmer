# See test-chat-tools.R for tests of tool calling

test_that("can get and set the system prompt", {
  chat <- chat_openai_test()
  chat$set_turns(
    list(
      UserTurn("Hi"),
      AssistantTurn("Hello")
    )
  )

  # NULL -> NULL
  chat$set_system_prompt(NULL)
  expect_equal(chat$get_system_prompt(), NULL)

  # NULL -> string
  chat$set_system_prompt("x")
  expect_equal(chat$get_system_prompt(), "x")

  # string -> string
  chat$set_system_prompt("y")
  expect_equal(chat$get_system_prompt(), "y")

  # string -> NULL
  chat$set_system_prompt(NULL)
  expect_equal(chat$get_system_prompt(), NULL)
})

test_that("system prompt can be a vector", {
  chat <- chat_openai_test(c("This is", "the system prompt"))
  expect_equal(chat$get_system_prompt(), "This is\n\nthe system prompt")
})

test_that("system prompt must be a character vector", {
  expect_snapshot(error = TRUE, {
    chat_openai_test(1)
  })
})

test_that("can retrieve system prompt with last_turn()", {
  chat1 <- chat_openai_test(system_prompt = NULL)
  expect_equal(chat1$last_turn("system"), NULL)

  chat2 <- chat_openai_test(system_prompt = "You are from New Zealand")
  expect_equal(
    chat2$last_turn("system"),
    SystemTurn("You are from New Zealand")
  )
})

test_that("can get and set turns", {
  chat <- chat_openai_test()
  expect_equal(chat$get_turns(), list())

  turns <- list(UserTurn(), AssistantTurn())
  chat$set_turns(turns)
  expect_equal(chat$get_turns(), list(UserTurn(), AssistantTurn()))
})

test_that("can get rounds, with and without system prompt", {
  chat <- chat_openai_test(system_prompt = "Be terse.")
  chat$set_turns(list(
    UserTurn("Hi"),
    AssistantTurn("Hello"),
    UserTurn("Bye"),
    AssistantTurn("Goodbye")
  ))

  rounds <- chat$get_rounds()
  expect_length(rounds, 2)
  expect_s7_class(rounds[[1]], Round)
  expect_equal(rounds[[1]]@input, list(UserTurn("Hi")))
  expect_equal(rounds[[2]]@input, list(UserTurn("Bye")))

  rounds <- chat$get_rounds(include_system_prompt = TRUE)
  expect_length(rounds, 2)
  expect_equal(
    rounds[[1]]@input,
    list(SystemTurn("Be terse."), UserTurn("Hi"))
  )
  expect_equal(rounds[[2]]@input, list(UserTurn("Bye")))
})

test_that("last_round() is NULL only when there are no turns at all", {
  chat <- chat_openai_test(system_prompt = NULL)
  expect_equal(chat$last_round(), NULL)
})

test_that("last_round() keeps system turns", {
  chat <- chat_openai_test(system_prompt = "Be terse.")
  expect_equal(chat$last_round()@input, list(SystemTurn("Be terse.")))
})

test_that("can retrieve last_round()", {
  chat <- chat_openai_test()

  chat$set_turns(list(
    UserTurn("Hi"),
    AssistantTurn("Hello"),
    UserTurn("Bye"),
    AssistantTurn("Goodbye")
  ))
  round <- chat$last_round()
  expect_s7_class(round, Round)
  expect_equal(round@input, list(UserTurn("Bye")))
  expect_equal(round@response, list(AssistantTurn("Goodbye")))
})

test_that("can get model", {
  chat <- chat_openai_test(model = "abc")
  expect_equal(chat$get_model(), "abc")
})

test_that("can set model", {
  chat <- chat_openai_test(model = "abc")
  chat$set_model("def")
  expect_equal(chat$get_model(), "def")
})

test_that("setting turns usually preserves, but can set system prompt", {
  chat <- chat_openai_test(system_prompt = "You're a funny guy")
  chat$set_turns(list())
  expect_equal(chat$get_system_prompt(), "You're a funny guy")

  chat$set_turns(list(SystemTurn(list(ContentText("You're a cool guy")))))
  expect_equal(chat$get_system_prompt(), "You're a cool guy")
})

test_that("can perform a simple batch chat", {
  chat <- chat_openai_test()

  result <- chat$chat("What's 1 + 1. Just give me the answer, no punctuation")
  expect_equal(result, ellmer_output("2"))
  expect_equal(chat$last_turn()@contents[[1]]@text, "2")
})

test_that("can chat with a single prompt", {
  chat <- chat_openai_test()
  expect_no_error(chat$chat(interpolate("What's 1 + 1?")))
})

test_that("can't chat with multiple prompts", {
  chat <- chat_openai_test()
  prompt <- interpolate("{{x}}", x = 1:2)
  expect_snapshot(error = TRUE, {
    chat$chat(prompt)
  })
})

test_that("can perform a simple async batch chat", {
  chat <- chat_openai_test()

  result <- chat$chat_async(
    "What's 1 + 1. Just give me the answer, no punctuation"
  )
  expect_s3_class(result, "promise")

  result <- sync(result)
  expect_equal(result, "2")
  expect_equal(chat$last_turn()@contents[[1]]@text, "2")
})

test_that("can perform a simple streaming chat", {
  chat <- chat_openai_test()

  chunks <- coro::collect(
    chat$stream(
      "
    What are the canonical colors of the ROYGBIV rainbow?
    Put each colour on its own line. Don't use punctuation.
  "
    )
  )
  expect_gt(length(chunks), 2)

  rainbow_re <- "^red *\norange *\nyellow *\ngreen *\nblue *\nindigo *\nviolet *\n?$"
  expect_match(paste(chunks, collapse = ""), rainbow_re, ignore.case = TRUE)
  expect_match(
    chat$last_turn()@contents[[1]]@text,
    rainbow_re,
    ignore.case = TRUE
  )
})

test_that("streaming handles multiple contents after merging each chunk", {
  make_response <- function() {
    coro::generator(function() {
      yield(list(type = "chunk"))
    })()
  }
  final_turn <- AssistantTurn(
    list(
      ContentText("hello"),
      ContentCitation(grounded_span = "hello")
    ),
    tokens = c(0, 0, 0),
    cost = 0
  )

  local_mocked_bindings(
    chat_perform = function(...) make_response(),
    stream_merge_chunks = function(provider, result, chunk) {
      list(merged = TRUE)
    },
    stream_content = function(provider, event, completion) {
      expect_true(completion$merged)
      list(
        ContentText("hello"),
        ContentCitation(grounded_span = "hello")
      )
    },
    value_finish_reason = function(provider, result) "success",
    value_turn = function(provider, model, result, has_type = FALSE) final_turn
  )

  content_chat <- Chat$new(test_provider(), model = test_model())
  content <- coro::collect(content_chat$stream("hi", stream = "content"))
  expect_s7_class(content[[1]], ContentText)
  expect_s7_class(content[[2]], ContentCitation)

  text_chat <- Chat$new(test_provider(), model = test_model())
  text <- coro::collect(text_chat$stream("hi", stream = "text"))
  expect_equal(paste0(unlist(text), collapse = ""), "hello\n")
})

test_that("provider hooks receive the exact request turns", {
  run_case <- function(async) {
    prior_turns <- list(
      UserTurn("First question"),
      AssistantTurn("First answer")
    )
    user_turn <- UserTurn("Follow-up question")
    expected_turns <- c(prior_turns, list(user_turn))
    content_turns <- list()
    value_turns <- list()
    final_turn <- AssistantTurn(
      list(ContentText("Follow-up answer")),
      tokens = c(0, 0, 0),
      cost = 0
    )
    make_response <- if (async) {
      function() {
        coro::async_generator(function() {
          yield(list(type = "chunk"))
        })()
      }
    } else {
      function() {
        coro::generator(function() {
          yield(list(type = "chunk"))
        })()
      }
    }

    local_mocked_bindings(
      chat_perform = function(..., turns) {
        expect_identical(turns, expected_turns)
        make_response()
      },
      stream_merge_chunks = function(provider, result, chunk) chunk,
      stream_content_with_turns = function(
        provider,
        event,
        completion,
        turns = list()
      ) {
        content_turns <<- turns
        list(ContentText("Follow-up answer"))
      },
      value_finish_reason = function(provider, result) "success",
      value_turn_with_turns = function(
        provider,
        model,
        result,
        has_type = FALSE,
        turns = list()
      ) {
        value_turns <<- turns
        final_turn
      }
    )

    chat <- Chat$new(test_provider(), model = test_model())
    chat$set_turns(prior_turns)
    generator <- if (async) {
      chat$.__enclos_env__$private$submit_turns_async(
        user_turn,
        stream = TRUE,
        echo = "none",
        controller = stream_controller()
      )
    } else {
      chat$.__enclos_env__$private$submit_turns(
        user_turn,
        stream = TRUE,
        echo = "none",
        controller = stream_controller()
      )
    }
    if (async) {
      sync(coro::async_collect(generator))
    } else {
      coro::collect(generator)
    }

    expect_identical(content_turns, expected_turns)
    expect_identical(value_turns, expected_turns)
  }

  run_case(FALSE)
  run_case(TRUE)
})

test_that("can perform a simple async batch chat", {
  chat <- chat_openai_test()

  chunks <- coro::async_collect(
    chat$stream_async(
      "
    What are the canonical colors of the ROYGBIV rainbow?
    Put each colour on its own line. Don't use punctuation.
  "
    )
  )
  expect_s3_class(chunks, "promise")

  chunks <- sync(chunks)
  expect_gt(length(chunks), 2)
  rainbow_re <- "^red *\norange *\nyellow *\ngreen *\nblue *\nindigo *\nviolet *\n?$"
  expect_match(paste(chunks, collapse = ""), rainbow_re, ignore.case = TRUE)
  expect_match(
    chat$last_turn()@contents[[1]]@text,
    rainbow_re,
    ignore.case = TRUE
  )
})

make_structured_stream_response <- function(
  deltas = c('{"name":"John"', ',"age":15}')
) {
  text <- paste(deltas, collapse = "")
  c(
    lapply(deltas, function(delta) {
      list(type = "response.output_text.delta", delta = delta)
    }),
    list(list(
      type = "response.completed",
      response = list(
        status = "completed",
        output = list(list(
          type = "message",
          content = list(list(
            type = "output_text",
            text = text
          ))
        )),
        usage = list(
          input_tokens = 1,
          output_tokens = 2,
          input_tokens_details = list(cached_tokens = 0)
        )
      )
    ))
  )
}

# Mock chat_perform to stream the given deltas; returns a function that
# reports the arguments chat_perform was called with
mock_chat_stream <- function(deltas, async, env = rlang::caller_env()) {
  response <- make_structured_stream_response(deltas)
  called_with <- NULL
  local_mocked_bindings(
    chat_perform = function(...) {
      called_with <<- list(...)
      gen <- if (async) coro::async_generator else coro::generator
      gen(function() {
        for (chunk in response) {
          yield(chunk)
        }
      })()
    },
    .env = env
  )
  function() called_with
}

collect_stream <- function(chat, ..., async) {
  if (async) {
    sync(coro::async_collect(chat$stream_async(...)))
  } else {
    coro::collect(chat$stream(...))
  }
}

make_text_stream_response <- function() {
  list(
    list(type = "response.output_text.delta", delta = "Hello"),
    list(
      type = "response.completed",
      response = list(
        status = "completed",
        output = list(list(
          type = "message",
          content = list(list(
            type = "output_text",
            text = "Hello"
          ))
        )),
        usage = list(
          input_tokens = 1,
          output_tokens = 2,
          input_tokens_details = list(cached_tokens = 0)
        )
      )
    )
  )
}

test_that("stream() supports native structured output", {
  person <- type_object(name = type_string(), age = type_integer())

  run_case <- function(async) {
    mock_chat_stream(c('{"name":"John"', ',"age":15}'), async)

    chat <- chat_openai_test()
    chunks <- collect_stream(
      chat,
      "Extract John, age 15",
      type = person,
      async = async
    )

    expect_identical(chunks, list('{"name":"John"', ',"age":15}'))
    expect_s7_class(chat$last_turn()@contents[[1]], ContentJson)
    expect_equal(
      chat$last_turn()@contents[[1]]@parsed,
      list(name = "John", age = 15)
    )
  }

  run_case(FALSE)
  run_case(TRUE)
})

test_that("stream() wraps scalar structured output types", {
  type <- type_string()

  run_case <- function(async) {
    called_with <- mock_chat_stream('{"wrapper":"John"}', async)

    chat <- chat_openai_test()
    chunks <- collect_stream(chat, "Extract John", type = type, async = async)

    requested_type <- called_with()$type
    expect_s7_class(requested_type, TypeObject)
    expect_identical(requested_type@properties, list(wrapper = type))
    expect_identical(chunks, list('{"wrapper":"John"}'))
    expect_s7_class(chat$last_turn()@contents[[1]], ContentJson)
    expect_equal(chat$last_turn()@contents[[1]]@parsed, list(wrapper = "John"))
  }

  run_case(FALSE)
  run_case(TRUE)
})

test_that("streaming structured output rejects unsupported providers and types", {
  withr::local_options(cli.width = 120)

  person <- type_object(name = type_string(), age = type_integer())
  additional_props <- suppressWarnings(
    type_object(value = type_string(), .additional_properties = TRUE)
  )
  nested_additional_props <- suppressWarnings(
    type_object(
      value = type_object(name = type_string(), .additional_properties = TRUE)
    )
  )

  run_case <- function(async) {
    # Errors should be raised before any request is made
    local_mocked_bindings(
      chat_perform = function(...) {
        stop("request should not have started")
      }
    )

    expect_snapshot(error = TRUE, {
      chat <- chat_anthropic_test(model = "claude-3-haiku-20240307")
      collect_stream(chat, "Extract John, age 15", type = person, async = async)
    })
    expect_snapshot(error = TRUE, {
      chat <- chat_anthropic_test(model = "claude-sonnet-5")
      collect_stream(
        chat,
        "Extract John",
        type = additional_props,
        async = async
      )
    })
    expect_snapshot(error = TRUE, {
      chat <- chat_anthropic_test(model = "claude-sonnet-5")
      collect_stream(
        chat,
        "Extract John",
        type = nested_additional_props,
        async = async
      )
    })
    # chat_aws_bedrock_test() skips when AWS credentials aren't available;
    # catch that here so the rest of the test still runs
    bedrock_chat <- tryCatch(
      chat_aws_bedrock_test(),
      skip = function(cnd) NULL
    )
    if (!is.null(bedrock_chat)) {
      expect_snapshot(error = TRUE, {
        collect_stream(
          bedrock_chat,
          "Extract John",
          type = person,
          async = async
        )
      })
    }
  }

  run_case(FALSE)
  run_case(TRUE)
})

test_that("structured text streams omit registered tools from requests", {
  run_case <- function(async) {
    request_started <- FALSE
    requested_tools <- NULL
    response <- make_structured_stream_response()

    local_mocked_bindings(
      chat_perform = function(...) {
        request_started <<- TRUE
        requested_tools <<- list(...)$tools
        if (async) {
          coro::async_generator(function() {
            for (chunk in response) {
              yield(chunk)
            }
          })()
        } else {
          coro::generator(function() {
            for (chunk in response) {
              yield(chunk)
            }
          })()
        }
      }
    )

    chat <- chat_openai_test()
    chat$register_tool(tool(function() "unused", "unused"))
    if (async) {
      sync(coro::async_collect(chat$stream_async(
        "Extract John",
        type = type_object(value = type_string())
      )))
    } else {
      coro::collect(chat$stream(
        "Extract John",
        type = type_object(value = type_string())
      ))
    }

    expect_true(request_started)
    expect_length(requested_tools %||% list(), 0)
  }

  run_case(FALSE)
  run_case(TRUE)
})

test_that("untyped streams retain registered tools in requests", {
  run_case <- function(async) {
    request_started <- FALSE
    requested_tools <- NULL
    response <- make_text_stream_response()

    local_mocked_bindings(
      chat_perform = function(...) {
        request_started <<- TRUE
        requested_tools <<- list(...)$tools
        if (async) {
          coro::async_generator(function() {
            for (chunk in response) {
              yield(chunk)
            }
          })()
        } else {
          coro::generator(function() {
            for (chunk in response) {
              yield(chunk)
            }
          })()
        }
      }
    )

    chat <- chat_openai_test()
    chat$register_tool(tool(function() "unused", "unused"))
    if (async) {
      sync(coro::async_collect(chat$stream_async("Hello")))
    } else {
      coro::collect(chat$stream("Hello"))
    }

    expect_true(request_started)
    expect_length(requested_tools, 1)
  }

  run_case(FALSE)
  run_case(TRUE)
})

test_that("structured content streams yield Content objects", {
  run_case <- function(async) {
    response <- make_structured_stream_response()

    local_mocked_bindings(
      chat_perform = function(...) {
        if (async) {
          coro::async_generator(function() {
            for (chunk in response) {
              yield(chunk)
            }
          })()
        } else {
          coro::generator(function() {
            for (chunk in response) {
              yield(chunk)
            }
          })()
        }
      }
    )

    chat <- chat_openai_test()
    chunks <- if (async) {
      sync(coro::async_collect(chat$stream_async(
        "Extract John",
        type = type_object(value = type_string()),
        stream = "content"
      )))
    } else {
      coro::collect(chat$stream(
        "Extract John",
        type = type_object(value = type_string()),
        stream = "content"
      ))
    }

    expect_length(chunks, 2)
    expect_s7_class(chunks[[1]], ContentText)
    expect_s7_class(chunks[[2]], ContentText)
    expect_identical(
      lapply(chunks, function(chunk) chunk@text),
      list('{"name":"John"', ',"age":15}')
    )
    expect_s7_class(chat$last_turn()@contents[[1]], ContentJson)
  }

  run_case(FALSE)
  run_case(TRUE)
})

test_that("can extract structured data", {
  person <- type_object(name = type_string(), age = type_integer())

  chat <- chat_openai_test()
  data <- chat$chat_structured("John, age 15, won first prize", type = person)
  expect_equal(data, list(name = "John", age = 15))
})

test_that("can extract structured data (async)", {
  person <- type_object(name = type_string(), age = type_integer())

  chat <- chat_openai_test()
  data <- sync(
    chat$chat_structured_async(
      "John, age 15, won first prize",
      type = person
    )
  )
  expect_equal(data, list(name = "John", age = 15))
})

test_that("chat_structured() doesn't require a prompt", {
  chat <- chat_openai_test()
  chat$chat("What's the biggest city in the world? What country is it in?")

  out <- chat$chat_structured(
    type = type_object(
      city = type_string(),
      county = type_string()
    )
  )
  expect_equal(out, list(city = "Tokyo", county = "Japan"))
})

test_that("has a basic print method", {
  chat <- chat_openai_test()
  chat$set_turns(
    list(
      UserTurn("What's 1 + 1?\nWhat's 1 + 2?"),
      AssistantTurn("2\n\n3", tokens = c(10, 5, 5))
    )
  )
  expect_snapshot(chat)
})

test_that("print method shows interrupted for partial turns", {
  chat <- chat_openai_test(model = "gpt-4o", system_prompt = NULL)
  chat$set_turns(
    list(
      UserTurn("Input 1"),
      AssistantTurn("Output 1", tokens = c(15000, 500, 0), cost = 0.2),
      UserTurn("Input 2"),
      AssistantPartialTurn("Partial output...")
    )
  )
  expect_snapshot(chat)
})

test_that("print method shows custom reason for partial turns", {
  chat <- chat_openai_test(model = "gpt-4o", system_prompt = NULL)
  chat$set_turns(
    list(
      UserTurn("Input 1"),
      AssistantPartialTurn("Partial output...", reason = "cancelled")
    )
  )
  expect_snapshot(chat)
})

test_that("print method shows cumulative tokens & cost", {
  chat <- chat_openai_test(model = "gpt-4o", system_prompt = NULL)
  chat$set_turns(
    list(
      UserTurn("Input 1"),
      AssistantTurn("Output 1", tokens = c(15000, 500, 0), cost = 0.2),
      UserTurn("Input 2"),
      AssistantTurn("Output 1", tokens = c(30000, 1000, 0), cost = 0.1)
    )
  )
  expect_snapshot(chat)
})

test_that("can compute costs", {
  chat <- chat_openai_test(model = "gpt-4o", system_prompt = NULL)
  chat$set_turns(
    list(
      UserTurn("Input 1"),
      AssistantTurn("Output 1", tokens = c(15000, 500, 0), cost = 0.2),
      UserTurn("Input 2"),
      AssistantTurn("Output 1", tokens = c(30000, 1000, 0), cost = 0.1)
    )
  )

  expect_equal(chat$get_cost(), dollars(0.3))
  expect_equal(chat$get_cost("last"), dollars(0.1))

  details <- chat$get_tokens()
  expect_equal(details$cost, dollars(c(0.2, 0.1)))
  expect_equal(details$input, c(15000, 30000))
  expect_equal(details$output, c(500, 1000))
  expect_equal(details$cached_input, c(0, 0))

  expect_snapshot(details)
})

test_that("can optionally echo", {
  chat <- chat_openai_test("Repeat the input back to me exactly", echo = TRUE)
  expect_output(chat$chat("Echo this."), "Echo this.")
  expect_output(chat$chat("Echo this.", echo = FALSE), NA)

  chat <- chat_openai_test("Repeat the input back to me exactly")
  expect_output(chat$chat("Echo this."), NA)
  expect_output(chat$chat("Echo this.", echo = TRUE), "Echo this.")
})

test_that("echo streams citation markers and a deduplicated footer", {
  first <- WebSource("https://example.com/first", "First")
  second <- WebSource("https://example.com/second", "Second")
  emitted <- ""

  response <- coro::generator(function() {
    yield(list(type = "text-one"))
    expect_equal(emitted, "First claim.")
    yield(list(type = "citation-one"))
    expect_equal(emitted, "First claim.[1]")
    yield(list(type = "text-two"))
    expect_equal(emitted, "First claim.[1] Second claim.")
    yield(list(type = "citation-two"))
    expect_equal(emitted, "First claim.[1] Second claim.[1]")
    yield(list(type = "citation-three"))
    expect_equal(emitted, "First claim.[1] Second claim.[1][2]")
  })()

  final_turn <- AssistantTurn(
    list(
      ContentText("First claim."),
      ContentCitation(source = first),
      ContentText(" Second claim."),
      ContentCitation(source = first),
      ContentCitation(source = second),
      ContentToolRequestSearch("citation test")
    ),
    tokens = c(0, 0, 0),
    cost = 0
  )

  local_mocked_bindings(
    chat_perform = function(...) response,
    stream_merge_chunks = function(provider, result, chunk) chunk,
    stream_content = function(provider, event, completion) {
      switch(
        event$type,
        "text-one" = list(final_turn@contents[[1]]),
        "citation-one" = list(final_turn@contents[[2]]),
        "text-two" = list(final_turn@contents[[3]]),
        "citation-two" = list(final_turn@contents[[4]]),
        "citation-three" = list(final_turn@contents[[5]])
      )
    },
    value_finish_reason = function(provider, result) "success",
    value_turn = function(provider, model, result, has_type = FALSE) final_turn,
    emitter = function(echo, prefix = NULL) {
      function(...) emitted <<- paste0(emitted, paste0(..., collapse = ""))
    }
  )

  chat <- Chat$new(test_provider(), model = test_model())
  chunks <- coro::collect(
    chat$.__enclos_env__$private$submit_turns(
      UserTurn("Question"),
      stream = TRUE,
      echo = "output",
      yield_as_content = TRUE,
      controller = stream_controller()
    )
  )

  expect_equal(
    emitted,
    paste0(
      "First claim.[1] Second claim.[1][2]\n\n",
      "Sources\n",
      "[1] First: https://example.com/first\n",
      "[2] Second: https://example.com/second\n"
    )
  )
  expect_equal(
    chunks,
    c(final_turn@contents[1:5], list(ContentText("\n")))
  )
})

test_that("async echo streams citation markers and a deduplicated footer", {
  first <- WebSource("https://example.com/first", "First")
  second <- WebSource("https://example.com/second", "Second")
  emitted <- ""
  assert_emitted <- function(expected) {
    if (!identical(emitted, expected)) {
      stop(
        "Expected `emitted` to equal ",
        encodeString(expected, quote = "\""),
        ", not ",
        encodeString(emitted, quote = "\""),
        ".",
        call. = FALSE
      )
    }
  }

  response <- coro::async_generator(function() {
    yield(list(type = "text-one"))
    assert_emitted("First claim.")
    yield(list(type = "citation-one"))
    assert_emitted("First claim.[1]")
    yield(list(type = "text-two"))
    assert_emitted("First claim.[1] Second claim.")
    yield(list(type = "citation-two"))
    assert_emitted("First claim.[1] Second claim.[1]")
    yield(list(type = "citation-three"))
    assert_emitted("First claim.[1] Second claim.[1][2]")
    coro::exhausted()
  })()

  final_turn <- AssistantTurn(
    list(
      ContentText("First claim."),
      ContentCitation(source = first),
      ContentText(" Second claim."),
      ContentCitation(source = first),
      ContentCitation(source = second),
      ContentToolRequestSearch("citation test")
    ),
    tokens = c(0, 0, 0),
    cost = 0
  )

  local_mocked_bindings(
    chat_perform = function(...) response,
    stream_merge_chunks = function(provider, result, chunk) chunk,
    stream_content = function(provider, event, completion) {
      switch(
        event$type,
        "text-one" = list(final_turn@contents[[1]]),
        "citation-one" = list(final_turn@contents[[2]]),
        "text-two" = list(final_turn@contents[[3]]),
        "citation-two" = list(final_turn@contents[[4]]),
        "citation-three" = list(final_turn@contents[[5]])
      )
    },
    value_finish_reason = function(provider, result) "success",
    value_turn = function(provider, model, result, has_type = FALSE) final_turn,
    emitter = function(echo, prefix = NULL) {
      function(...) emitted <<- paste0(emitted, paste0(..., collapse = ""))
    }
  )

  chat <- Chat$new(test_provider(), model = test_model())
  expect_no_error(
    sync(
      coro::async_collect(
        chat$.__enclos_env__$private$submit_turns_async(
          UserTurn("Question"),
          stream = TRUE,
          echo = "output",
          controller = stream_controller()
        )
      )
    )
  )

  expect_equal(
    emitted,
    paste0(
      "First claim.[1] Second claim.[1][2]\n\n",
      "Sources\n",
      "[1] First: https://example.com/first\n",
      "[2] Second: https://example.com/second\n"
    )
  )
})

test_that("streaming echo does not leak thinking content", {
  run_case <- function(async, echo) {
    emitted <- ""
    make_response <- if (async) {
      function() {
        coro::async_generator(function() {
          yield(list(type = "content"))
        })()
      }
    } else {
      function() {
        coro::generator(function() {
          yield(list(type = "content"))
        })()
      }
    }
    final_turn <- AssistantTurn(
      list(
        ContentThinking("Internal reasoning."),
        ContentText("Visible answer.")
      ),
      tokens = c(0, 0, 0),
      cost = 0
    )

    local_mocked_bindings(
      chat_perform = function(...) make_response(),
      stream_merge_chunks = function(provider, result, chunk) chunk,
      stream_content = function(provider, event, completion) {
        final_turn@contents
      },
      value_finish_reason = function(provider, result) "success",
      value_turn = function(provider, model, result, has_type = FALSE) {
        final_turn
      },
      emitter = function(echo, prefix = NULL) {
        function(...) emitted <<- paste0(emitted, paste0(..., collapse = ""))
      }
    )

    chat <- Chat$new(test_provider(), model = test_model())
    generator <- if (async) {
      chat$.__enclos_env__$private$submit_turns_async(
        UserTurn("Question"),
        stream = TRUE,
        echo = echo,
        yield_as_content = TRUE,
        controller = stream_controller()
      )
    } else {
      chat$.__enclos_env__$private$submit_turns(
        UserTurn("Question"),
        stream = TRUE,
        echo = echo,
        yield_as_content = TRUE,
        controller = stream_controller()
      )
    }
    output <- capture.output(
      chunks <- if (async) {
        sync(coro::async_collect(generator))
      } else {
        coro::collect(generator)
      }
    )

    expect_equal(emitted, "Visible answer.\n")
    expect_equal(
      chunks,
      c(final_turn@contents, list(ContentText("\n")))
    )
    thinking_output <- output[startsWith(output, "< ")]
    if (echo == "all") {
      expect_equal(
        thinking_output,
        c(
          "< <thinking>",
          "< Internal reasoning.",
          "< </thinking>"
        )
      )
    } else {
      expect_equal(thinking_output, character())
    }
  }

  run_case(FALSE, "output")
  run_case(FALSE, "all")
  run_case(TRUE, "output")
  run_case(TRUE, "all")
})

test_that("streaming citation footer follows emitted marker newline state", {
  run_case <- function(async) {
    emitted <- ""
    source <- WebSource("https://example.com", "Example")
    final_turn <- AssistantTurn(
      list(
        ContentText("Answer.\n"),
        ContentCitation(source = source)
      ),
      tokens = c(0, 0, 0),
      cost = 0
    )
    make_response <- if (async) {
      function() {
        coro::async_generator(function() {
          yield(list(type = "content"))
        })()
      }
    } else {
      function() {
        coro::generator(function() {
          yield(list(type = "content"))
        })()
      }
    }

    local_mocked_bindings(
      chat_perform = function(...) make_response(),
      stream_merge_chunks = function(provider, result, chunk) chunk,
      stream_content = function(provider, event, completion) {
        final_turn@contents
      },
      value_finish_reason = function(provider, result) "success",
      value_turn = function(provider, model, result, has_type = FALSE) {
        final_turn
      },
      emitter = function(echo, prefix = NULL) {
        function(...) emitted <<- paste0(emitted, paste0(..., collapse = ""))
      }
    )

    chat <- Chat$new(test_provider(), model = test_model())
    generator <- if (async) {
      chat$.__enclos_env__$private$submit_turns_async(
        UserTurn("Question"),
        stream = TRUE,
        echo = "output",
        yield_as_content = TRUE,
        controller = stream_controller()
      )
    } else {
      chat$.__enclos_env__$private$submit_turns(
        UserTurn("Question"),
        stream = TRUE,
        echo = "output",
        yield_as_content = TRUE,
        controller = stream_controller()
      )
    }
    chunks <- if (async) {
      sync(coro::async_collect(generator))
    } else {
      coro::collect(generator)
    }

    expect_equal(
      emitted,
      paste0(
        "Answer.\n[1]\n\n",
        "Sources\n",
        "[1] Example: https://example.com\n"
      )
    )
    expect_equal(chunks, final_turn@contents)
  }

  run_case(FALSE)
  run_case(TRUE)
})

test_that("citation-only streaming output terminates before footer", {
  run_case <- function(async) {
    emitted <- ""
    citation <- ContentCitation(
      source = WebSource("https://example.com", "Example")
    )
    final_turn <- AssistantTurn(
      list(citation),
      tokens = c(0, 0, 0),
      cost = 0
    )
    make_response <- if (async) {
      function() {
        coro::async_generator(function() {
          yield(list(type = "content"))
        })()
      }
    } else {
      function() {
        coro::generator(function() {
          yield(list(type = "content"))
        })()
      }
    }

    local_mocked_bindings(
      chat_perform = function(...) make_response(),
      stream_merge_chunks = function(provider, result, chunk) chunk,
      stream_content = function(provider, event, completion) {
        final_turn@contents
      },
      value_finish_reason = function(provider, result) "success",
      value_turn = function(provider, model, result, has_type = FALSE) {
        final_turn
      },
      emitter = function(echo, prefix = NULL) {
        function(...) emitted <<- paste0(emitted, paste0(..., collapse = ""))
      }
    )

    chat <- Chat$new(test_provider(), model = test_model())
    generator <- if (async) {
      chat$.__enclos_env__$private$submit_turns_async(
        UserTurn("Question"),
        stream = TRUE,
        echo = "output",
        yield_as_content = TRUE,
        controller = stream_controller()
      )
    } else {
      chat$.__enclos_env__$private$submit_turns(
        UserTurn("Question"),
        stream = TRUE,
        echo = "output",
        yield_as_content = TRUE,
        controller = stream_controller()
      )
    }
    chunks <- if (async) {
      sync(coro::async_collect(generator))
    } else {
      coro::collect(generator)
    }

    expect_equal(
      emitted,
      paste0(
        "[1]\n\n",
        "Sources\n",
        "[1] Example: https://example.com\n"
      )
    )
    expect_equal(chunks, list(citation))
  }

  run_case(FALSE)
  run_case(TRUE)
})

test_that("empty streaming text preserves emitted newline state", {
  run_case <- function(async) {
    emitted <- ""
    final_turn <- AssistantTurn(
      list(
        ContentText("Answer.\n"),
        ContentText("")
      ),
      tokens = c(0, 0, 0),
      cost = 0
    )
    make_response <- if (async) {
      function() {
        coro::async_generator(function() {
          yield(list(type = "content"))
        })()
      }
    } else {
      function() {
        coro::generator(function() {
          yield(list(type = "content"))
        })()
      }
    }

    local_mocked_bindings(
      chat_perform = function(...) make_response(),
      stream_merge_chunks = function(provider, result, chunk) chunk,
      stream_content = function(provider, event, completion) {
        final_turn@contents
      },
      value_finish_reason = function(provider, result) "success",
      value_turn = function(provider, model, result, has_type = FALSE) {
        final_turn
      },
      emitter = function(echo, prefix = NULL) {
        function(...) emitted <<- paste0(emitted, paste0(..., collapse = ""))
      }
    )

    chat <- Chat$new(test_provider(), model = test_model())
    generator <- if (async) {
      chat$.__enclos_env__$private$submit_turns_async(
        UserTurn("Question"),
        stream = TRUE,
        echo = "output",
        yield_as_content = TRUE,
        controller = stream_controller()
      )
    } else {
      chat$.__enclos_env__$private$submit_turns(
        UserTurn("Question"),
        stream = TRUE,
        echo = "output",
        yield_as_content = TRUE,
        controller = stream_controller()
      )
    }
    chunks <- if (async) {
      sync(coro::async_collect(generator))
    } else {
      coro::collect(generator)
    }

    expect_equal(
      emitted,
      "Answer.\n"
    )
    expect_equal(chunks, final_turn@contents)
  }

  run_case(FALSE)
  run_case(TRUE)
})

test_that("non-streaming echo preserves citation yields", {
  run_case <- function(async) {
    emitted <- ""
    source <- WebSource("https://example.com", "Example")
    final_turn <- AssistantTurn(
      list(
        ContentText("Complete answer."),
        ContentCitation(source = source),
        ContentCitation(source = source)
      ),
      tokens = c(0, 0, 0),
      cost = 0
    )
    response <- list()

    local_mocked_bindings(
      chat_perform = function(provider, mode, ...) {
        if (mode == "async-value") {
          promises::promise_resolve(response)
        } else {
          response
        }
      },
      resp_body_json = function(response) list(),
      resp_timing = function(response) c(total = 0),
      value_finish_reason = function(provider, result) "success",
      value_turn = function(provider, model, result, has_type = FALSE) {
        final_turn
      },
      emitter = function(echo, prefix = NULL) {
        function(...) emitted <<- paste0(emitted, paste0(..., collapse = ""))
      }
    )

    chat <- Chat$new(test_provider(), model = test_model())
    generator <- if (async) {
      chat$.__enclos_env__$private$submit_turns_async(
        UserTurn("Question"),
        stream = FALSE,
        echo = "output",
        yield_as_content = TRUE,
        controller = stream_controller()
      )
    } else {
      chat$.__enclos_env__$private$submit_turns(
        UserTurn("Question"),
        stream = FALSE,
        echo = "output",
        yield_as_content = TRUE,
        controller = stream_controller()
      )
    }
    chunks <- if (async) {
      sync(coro::async_collect(generator))
    } else {
      coro::collect(generator)
    }

    expect_equal(
      emitted,
      paste0(
        "Complete answer.[1][1]\n\n",
        "Sources\n",
        "[1] Example: https://example.com\n"
      )
    )
    expect_equal(
      chunks,
      list(ContentText("Complete answer."), ContentText("\n"))
    )
  }

  run_case(FALSE)
  run_case(TRUE)
})

test_that("async echo none stays silent and preserves rich content", {
  emitted <- ""
  citation <- ContentCitation(
    source = WebSource("https://example.com", "Example")
  )
  response <- coro::async_generator(function() {
    yield(list(type = "content"))
  })()
  final_turn <- AssistantTurn(
    list(
      ContentText("Silent answer."),
      citation
    ),
    tokens = c(0, 0, 0),
    cost = 0
  )

  local_mocked_bindings(
    chat_perform = function(...) response,
    stream_merge_chunks = function(provider, result, chunk) chunk,
    stream_content = function(provider, event, completion) {
      final_turn@contents
    },
    value_finish_reason = function(provider, result) "success",
    value_turn = function(provider, model, result, has_type = FALSE) final_turn,
    emitter = function(echo, prefix = NULL) {
      if (echo == "none") {
        return(function(...) invisible())
      }
      function(...) emitted <<- paste0(emitted, paste0(..., collapse = ""))
    }
  )

  chat <- Chat$new(test_provider(), model = test_model())
  chunks <- sync(
    coro::async_collect(
      chat$.__enclos_env__$private$submit_turns_async(
        UserTurn("Question"),
        stream = TRUE,
        echo = "none",
        yield_as_content = TRUE,
        controller = stream_controller()
      )
    )
  )

  expect_equal(emitted, "")
  expect_equal(
    chunks,
    c(final_turn@contents, list(ContentText("\n")))
  )
})

test_that("async cancelled echo retains citation markers and sources", {
  controller <- stream_controller()
  log_count <- 0L
  emitted <- ""
  text <- ContentText("Partial grounded answer.")
  citation <- ContentCitation(
    source = WebSource("https://example.com", "Example"),
    grounded_span = "grounded answer"
  )
  make_response <- function() {
    coro::async_generator(function() {
      yield(list(type = "partial"))
    })()
  }

  local_mocked_bindings(
    chat_perform = function(...) make_response(),
    stream_merge_chunks = function(provider, result, chunk) chunk,
    stream_content = function(provider, event, completion) {
      controller$cancel()
      list(
        text,
        citation
      )
    },
    log_turn = function(provider, model, turn) log_count <<- log_count + 1L,
    emitter = function(echo, prefix = NULL) {
      function(...) emitted <<- paste0(emitted, paste0(..., collapse = ""))
    }
  )

  chat <- Chat$new(test_provider(), model = test_model())
  chunks <- sync(
    coro::async_collect(
      chat$.__enclos_env__$private$submit_turns_async(
        UserTurn("Stop after this chunk."),
        stream = TRUE,
        echo = "output",
        yield_as_content = TRUE,
        controller = controller
      )
    )
  )

  expect_equal(
    emitted,
    paste0(
      "Partial grounded answer.[1]\n\n",
      "Sources\n",
      "[1] Example: https://example.com\n"
    )
  )
  expect_s7_class(chat$last_turn(), AssistantPartialTurn)
  expect_equal(log_count, 1L)
  expect_equal(chunks, list(text, citation))
})

test_that("echo summarizes web response records without raw output", {
  run_case <- function(async) {
    make_response <- if (async) {
      function() {
        coro::async_generator(function() {
          yield(list(type = "text"))
        })()
      }
    } else {
      function() {
        coro::generator(function() {
          yield(list(type = "text"))
        })()
      }
    }
    final_turn <- AssistantTurn(
      list(
        ContentText("Web answer."),
        ContentToolRequestSearch("ellmer web activity"),
        ContentToolRequestFetch("https://example.com/request"),
        ContentToolResponseSearch(
          sources = list(
            WebSource("https://example.com/search-1", "Search result 1")
          )
        ),
        ContentToolResponseSearch(
          sources = list(
            WebSource("https://example.com/search-2", "Search result 2")
          )
        ),
        ContentToolResponseFetch(
          url = "https://example.com/fetch-1",
          status = "success"
        ),
        ContentToolResponseFetch(
          url = "https://example.com/fetch-2",
          status = "success"
        ),
        ContentThinking("Internal reasoning.")
      ),
      tokens = c(0, 0, 0),
      cost = 0
    )

    local_mocked_bindings(
      chat_perform = function(...) make_response(),
      stream_merge_chunks = function(provider, result, chunk) chunk,
      stream_content = function(provider, event, completion) {
        list(ContentText("Web answer."))
      },
      value_finish_reason = function(provider, result) "success",
      value_turn = function(provider, model, result, has_type = FALSE) {
        final_turn
      }
    )

    chat <- Chat$new(test_provider(), model = test_model())
    generator <- if (async) {
      chat$.__enclos_env__$private$submit_turns_async(
        UserTurn("Summarize web activity."),
        stream = TRUE,
        echo = "all",
        controller = stream_controller()
      )
    } else {
      chat$.__enclos_env__$private$submit_turns(
        UserTurn("Summarize web activity."),
        stream = TRUE,
        echo = "all",
        controller = stream_controller()
      )
    }
    output <- capture.output(
      invisible(
        if (async) {
          sync(coro::async_collect(generator))
        } else {
          coro::collect(generator)
        }
      )
    )
    output <- gsub("^< ", "", output)
    output <- paste(output, collapse = "\n")

    expect_match(output, "Web activity: 2 searches, 2 fetches", fixed = TRUE)
    expect_false(grepl("[web search results]:", output, fixed = TRUE))
    expect_false(grepl("[web fetch result]:", output, fixed = TRUE))
    expect_false(grepl("[web search request]:", output, fixed = TRUE))
    expect_false(grepl("[web fetch request]:", output, fixed = TRUE))
    expect_match(
      output,
      "<thinking>\nInternal reasoning.\n</thinking>",
      fixed = TRUE
    )
  }

  run_case(FALSE)
  run_case(TRUE)
})

test_that("format_web_activity counts response-only providers", {
  turn <- AssistantTurn(
    list(
      ContentText("Web answer."),
      ContentToolResponseSearch(sources = list()),
      ContentToolResponseFetch(
        url = "https://example.com/fetch",
        status = "success"
      )
    )
  )

  expect_match(
    format_web_activity(turn@contents, TRUE),
    "Web activity: 1 search, 1 fetch",
    fixed = TRUE
  )
})

test_that("echo preserves one trailing newline for complete turns", {
  run_case <- function(async) {
    emitted <- ""
    final_turn <- AssistantTurn(
      list(ContentText("Complete answer.\n")),
      tokens = c(0, 0, 0),
      cost = 0
    )
    make_response <- if (async) {
      function() {
        coro::async_generator(function() {
          yield(list(type = "text"))
        })()
      }
    } else {
      function() {
        coro::generator(function() {
          yield(list(type = "text"))
        })()
      }
    }

    local_mocked_bindings(
      chat_perform = function(...) make_response(),
      stream_merge_chunks = function(provider, result, chunk) chunk,
      stream_content = function(provider, event, completion) {
        list(ContentText("Complete answer.\n"))
      },
      value_finish_reason = function(provider, result) "success",
      value_turn = function(provider, model, result, has_type = FALSE) {
        final_turn
      },
      emitter = function(echo, prefix = NULL) {
        function(...) emitted <<- paste0(emitted, paste0(..., collapse = ""))
      }
    )

    chat <- Chat$new(test_provider(), model = test_model())
    generator <- if (async) {
      chat$.__enclos_env__$private$submit_turns_async(
        UserTurn("Give a complete answer."),
        stream = TRUE,
        echo = "output",
        yield_as_content = TRUE,
        controller = stream_controller()
      )
    } else {
      chat$.__enclos_env__$private$submit_turns(
        UserTurn("Give a complete answer."),
        stream = TRUE,
        echo = "output",
        yield_as_content = TRUE,
        controller = stream_controller()
      )
    }
    chunks <- if (async) {
      sync(coro::async_collect(generator))
    } else {
      coro::collect(generator)
    }

    expect_equal(emitted, "Complete answer.\n")
    expect_equal(chunks, list(ContentText("Complete answer.\n")))
  }

  run_case(FALSE)
  run_case(TRUE)
})

test_that("echo preserves one trailing newline for partial turns", {
  run_case <- function(async) {
    controller <- stream_controller()
    emitted <- ""
    make_response <- if (async) {
      function() {
        coro::async_generator(function() {
          yield(list(type = "text"))
        })()
      }
    } else {
      function() {
        coro::generator(function() {
          yield(list(type = "text"))
        })()
      }
    }

    local_mocked_bindings(
      chat_perform = function(...) make_response(),
      stream_merge_chunks = function(provider, result, chunk) chunk,
      stream_content = function(provider, event, completion) {
        controller$cancel()
        list(ContentText("Partial answer.\n"))
      },
      emitter = function(echo, prefix = NULL) {
        function(...) emitted <<- paste0(emitted, paste0(..., collapse = ""))
      }
    )

    chat <- Chat$new(test_provider(), model = test_model())
    generator <- if (async) {
      chat$.__enclos_env__$private$submit_turns_async(
        UserTurn("Stop after this chunk."),
        stream = TRUE,
        echo = "output",
        yield_as_content = TRUE,
        controller = controller
      )
    } else {
      chat$.__enclos_env__$private$submit_turns(
        UserTurn("Stop after this chunk."),
        stream = TRUE,
        echo = "output",
        yield_as_content = TRUE,
        controller = controller
      )
    }
    chunks <- if (async) {
      sync(coro::async_collect(generator))
    } else {
      coro::collect(generator)
    }

    expect_equal(emitted, "Partial answer.\n")
    expect_equal(chunks, list(ContentText("Partial answer.\n")))
  }

  run_case(FALSE)
  run_case(TRUE)
})

test_that("cancelled echo retains citation markers and sources", {
  controller <- stream_controller()
  log_count <- 0L
  emitted <- ""
  text <- ContentText("Partial grounded answer.")
  citation <- ContentCitation(
    source = WebSource("https://example.com", "Example"),
    grounded_span = "grounded answer"
  )
  make_response <- function() {
    coro::generator(function() {
      yield(list(type = "partial"))
    })()
  }

  local_mocked_bindings(
    chat_perform = function(...) make_response(),
    stream_merge_chunks = function(provider, result, chunk) chunk,
    stream_content = function(provider, event, completion) {
      controller$cancel()
      list(
        text,
        citation
      )
    },
    log_turn = function(provider, model, turn) log_count <<- log_count + 1L,
    emitter = function(echo, prefix = NULL) {
      function(...) emitted <<- paste0(emitted, paste0(..., collapse = ""))
    }
  )

  chat <- Chat$new(test_provider(), model = test_model())
  chunks <- coro::collect(
    chat$.__enclos_env__$private$submit_turns(
      UserTurn("Stop after this chunk."),
      stream = TRUE,
      echo = "output",
      yield_as_content = TRUE,
      controller = controller
    )
  )

  expect_equal(
    emitted,
    paste0(
      "Partial grounded answer.[1]\n\n",
      "Sources\n",
      "[1] Example: https://example.com\n"
    )
  )
  expect_s7_class(chat$last_turn(), AssistantPartialTurn)
  expect_length(chat$get_turns(), 2)
  expect_equal(log_count, 1L)
  expect_equal(chunks, list(text, citation))
})

test_that("echo ignores source-less citations", {
  emitted <- ""
  response <- coro::generator(function() {
    yield(list(type = "content"))
  })()
  final_turn <- AssistantTurn(
    list(
      ContentText("Grounded but source-less."),
      ContentCitation(grounded_span = "Grounded but source-less.")
    ),
    tokens = c(0, 0, 0),
    cost = 0
  )

  local_mocked_bindings(
    chat_perform = function(...) response,
    stream_merge_chunks = function(provider, result, chunk) chunk,
    stream_content = function(provider, event, completion) {
      final_turn@contents
    },
    value_finish_reason = function(provider, result) "success",
    value_turn = function(provider, model, result, has_type = FALSE) final_turn,
    emitter = function(echo, prefix = NULL) {
      function(...) emitted <<- paste0(emitted, paste0(..., collapse = ""))
    }
  )

  chat <- Chat$new(test_provider(), model = test_model())
  invisible(
    coro::collect(
      chat$.__enclos_env__$private$submit_turns(
        UserTurn("Question"),
        stream = TRUE,
        echo = "output",
        controller = stream_controller()
      )
    )
  )

  expect_equal(emitted, "Grounded but source-less.\n")
  expect_false(grepl("Sources", emitted, fixed = TRUE))
  expect_false(grepl("[1]", emitted, fixed = TRUE))
})

test_that("complete echo ends with one newline", {
  emitted <- ""
  final_turn <- AssistantTurn(
    list(ContentText("Complete answer.")),
    tokens = c(0, 0, 0),
    cost = 0
  )
  make_response <- function() {
    coro::generator(function() {
      yield(list(type = "text"))
    })()
  }

  local_mocked_bindings(
    chat_perform = function(...) make_response(),
    stream_merge_chunks = function(provider, result, chunk) chunk,
    stream_content = function(provider, event, completion) {
      list(ContentText("Complete answer."))
    },
    value_finish_reason = function(provider, result) "success",
    value_turn = function(provider, model, result, has_type = FALSE) final_turn,
    emitter = function(echo, prefix = NULL) {
      function(...) emitted <<- paste0(emitted, paste0(..., collapse = ""))
    }
  )

  chat <- Chat$new(test_provider(), model = test_model())
  invisible(
    coro::collect(
      chat$.__enclos_env__$private$submit_turns(
        UserTurn("Give a complete answer."),
        stream = TRUE,
        echo = "output",
        controller = stream_controller()
      )
    )
  )

  expect_equal(emitted, "Complete answer.\n")
})

test_that("can retrieve last_turn for user and assistant", {
  chat <- chat_openai_test()
  expect_equal(chat$last_turn("user"), NULL)
  expect_equal(chat$last_turn("assistant"), NULL)

  chat$chat("Hi")
  expect_equal(chat$last_turn("user")@role, "user")
  expect_equal(chat$last_turn("assistant")@role, "assistant")
})

test_that("api_headers parameter works correctly", {
  chat <- chat_openai_test(api_headers = c("X-Test" = "value"))
  expect_equal(chat$get_provider()@extra_headers, c("X-Test" = "value"))

  req <- chat_request(chat$get_provider(), chat$get_model_object())
  expect_equal(req_get_headers(req), list("X-Test" = "value"))
})

test_that("assistant turns track duration", {
  vcr::local_cassette("chat-duration")

  chat <- chat_openai_test()
  chat$chat("What's 1 + 1?")

  assistant_turn <- chat$last_turn()

  # These assistant durations are usually not NA, but are during replay (#479)
  expect_true(is.na(assistant_turn@duration) || assistant_turn@duration > 0)
})

# stream_controller() ----------------------------------------------------------

test_that("stream_controller() creates correct object", {
  ctrl <- stream_controller()
  expect_s3_class(ctrl, "ellmer_stream_controller")
  expect_false(ctrl$cancelled)
  expect_null(ctrl$reason)
  expect_true(is.function(ctrl$cancel))
  expect_true(is.function(ctrl$reset))
})

test_that("stream_controller()$cancel() sets cancelled to TRUE", {
  ctrl <- stream_controller()
  ctrl$cancel()
  expect_true(ctrl$cancelled)
  expect_equal(ctrl$reason, "cancelled")
})

test_that("stream_controller()$cancel() accepts a custom reason", {
  ctrl <- stream_controller()
  ctrl$cancel(reason = "timeout")
  expect_true(ctrl$cancelled)
  expect_equal(ctrl$reason, "timeout")
})

test_that("stream_controller()$reset() clears cancelled state and reason", {
  ctrl <- stream_controller()
  ctrl$cancel(reason = "timeout")
  expect_true(ctrl$cancelled)
  expect_equal(ctrl$reason, "timeout")
  ctrl$reset()
  expect_false(ctrl$cancelled)
  expect_null(ctrl$reason)
})

test_that("as_controller() resets a pre-cancelled controller", {
  ctrl <- stream_controller()
  ctrl$cancel()
  result <- expect_silent(as_controller(ctrl, reset = TRUE))
  expect_false(ctrl$cancelled)
  expect_identical(result, ctrl)
})

test_that("stream() rejects non-controller object", {
  chat <- chat_openai_test()
  expect_snapshot(error = TRUE, {
    coro::collect(chat$stream("hi", controller = TRUE))
  })
})

test_that("stream_async() rejects non-controller object", {
  chat <- chat_openai_test()
  expect_snapshot(error = TRUE, {
    sync(coro::async_collect(chat$stream_async("hi", controller = list())))
  })
})

test_that("as_controller() accepts a valid stream_controller() or NULL", {
  ctrl <- stream_controller()
  expect_identical(as_controller(ctrl), ctrl)

  default <- as_controller(NULL)
  expect_false(default$cancelled)
  expect_null(default$reason)
})

test_that("stream_controller() rejects invalid cancelled values", {
  ctrl <- stream_controller()
  expect_error(ctrl$cancelled <- "banana")
  expect_error(ctrl$cancelled <- NA)
  expect_error(ctrl$cancelled <- c(TRUE, FALSE))
})

test_that("stream_controller() rejects invalid reason values", {
  ctrl <- stream_controller()
  expect_error(ctrl$reason <- 123)
  expect_error(ctrl$reason <- NA_character_)
  expect_error(ctrl$reason <- c("a", "b"))
})

test_that("stream_controller() environment is locked", {
  ctrl <- stream_controller()
  expect_error(ctrl$typo <- TRUE)
})

test_that("finalize_turn() merges adjacent ContentText", {
  chat <- chat_openai_test()
  acc <- TurnAccumulator$new(
    chat,
    chat$.__enclos_env__$private,
    stream_controller()
  )

  user_turn <- Turn("user", list(ContentText("hi")))
  acc$begin_turn(user_turn)
  acc$update_turn(ContentText("Hello "))
  acc$update_turn(ContentText("world"))
  acc$finalize_turn()

  turn <- chat$last_turn()
  expect_s7_class(turn, AssistantPartialTurn)
  expect_length(turn@contents, 1)
  expect_equal(turn@text, "Hello world")
  expect_equal(turn@reason, "interrupted")
  # No token data
  expect_true(all(is.na(turn@tokens)))
  expect_true(is.na(turn@cost))
})

test_that("finalize_turn() uses controller reason", {
  chat <- chat_openai_test()
  ctrl <- stream_controller()
  ctrl$cancel(reason = "timeout")
  acc <- TurnAccumulator$new(chat, chat$.__enclos_env__$private, ctrl)

  user_turn <- Turn("user", list(ContentText("hi")))
  acc$begin_turn(user_turn)
  acc$update_turn(ContentText("partial"))
  acc$finalize_turn()

  turn <- chat$last_turn()
  expect_s7_class(turn, AssistantPartialTurn)
  expect_equal(turn@reason, "timeout")
})

test_that("finalize_turn() is a no-op for complete turns", {
  chat <- chat_openai_test()
  acc <- TurnAccumulator$new(
    chat,
    chat$.__enclos_env__$private,
    stream_controller()
  )

  user_turn <- Turn("user", list(ContentText("hi")))
  chat$add_turn(
    user_turn,
    AssistantTurn(contents = list(ContentText("done"))),
    log_tokens = FALSE
  )
  # Manually set turn_idx so finalize_turn has something to check
  acc$.__enclos_env__$private$turn_idx <- 2L

  acc$finalize_turn()
  turn <- chat$last_turn()

  expect_s7_class(turn, AssistantTurn)
  expect_false(S7_inherits(turn, AssistantPartialTurn))
})

test_that("update_turn() appends content incrementally", {
  chat <- chat_openai_test()
  acc <- TurnAccumulator$new(
    chat,
    chat$.__enclos_env__$private,
    stream_controller()
  )

  user_turn <- Turn("user", list(ContentText("hi")))
  acc$begin_turn(user_turn)
  acc$update_turn(ContentText("a"))
  acc$update_turn(ContentText("b"))

  turn <- chat$last_turn()
  expect_length(turn@contents, 2)
  expect_equal(turn@contents[[1]]@text, "a")
  expect_equal(turn@contents[[2]]@text, "b")
})

test_that("merge_content_text() merges adjacent text, preserves non-text", {
  contents <- list(
    ContentText("a"),
    ContentText("b"),
    ContentThinking("thought"),
    ContentText("c")
  )
  merged <- merge_content_text(contents)

  expect_length(merged, 3)
  expect_s7_class(merged[[1]], ContentText)
  expect_equal(merged[[1]]@text, "ab")
  expect_s7_class(merged[[2]], ContentThinking)
  expect_s7_class(merged[[3]], ContentText)
  expect_equal(merged[[3]]@text, "c")
})

test_that("tool_context()$turns includes the system prompt", {
  chat <- chat_openai_test(system_prompt = "Be terse.")

  captured <- NULL
  capture <- tool(
    function() {
      captured <<- tool_context()$turns
      "ok"
    },
    name = "capture",
    description = "Capture the tool context turns"
  )
  chat$register_tool(capture)

  turn <- Turn(
    "assistant",
    list(ContentToolRequest(id = "1", name = "capture", tool = capture))
  )
  coro::collect(invoke_tools(
    turn,
    tool_context = function(request) {
      new_tool_context(
        request,
        chat$get_turns(include_system_prompt = TRUE)
      )
    }
  ))

  expect_equal(captured[[1]]@role, "system")
})
