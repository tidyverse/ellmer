fake_codex_jwt <- function(account_id = "acct_test") {
  b64url <- function(x) {
    x <- jsonlite::base64_enc(charToRaw(x))
    x <- gsub("\\s+", "", x)
    x <- chartr("+/", "-_", x)
    sub("=+$", "", x)
  }
  payload <- jsonlite::toJSON(
    list(`https://api.openai.com/auth` = list(chatgpt_account_id = account_id)),
    auto_unbox = TRUE
  )
  paste(b64url('{"alg":"none"}'), b64url(payload), "sig", sep = ".")
}

test_that("codex credentials extract ChatGPT account id", {
  token <- fake_codex_jwt("acct_123")
  credentials <- openai_codex_credentials(access_token = token)

  expect_equal(openai_codex_account_id(token), "acct_123")
  expect_equal(
    credentials(),
    list(
      Authorization = paste("Bearer", token),
      `chatgpt-account-id` = "acct_123"
    )
  )
})

test_that("codex responses body uses instructions and codex defaults", {
  token <- fake_codex_jwt()
  chat <- chat_openai_codex_responses(
    system_prompt = "Be terse.",
    credentials = openai_codex_credentials(access_token = token),
    model = "gpt-5.5",
    echo = "none"
  )
  provider <- chat$get_provider()

  body <- chat_body(
    provider,
    stream = TRUE,
    turns = list(SystemTurn("Be terse."), UserTurn("Hi"))
  )

  expect_equal(body$model, "gpt-5.5")
  expect_equal(body$instructions, "Be terse.")
  expect_equal(body$input[[1]]$role, "user")
  expect_equal(body$input[[1]]$content[[1]]$type, "input_text")
  expect_equal(body$text$verbosity, "low")
  expect_false(body$store)
  expect_equal(body$include, list("reasoning.encrypted_content"))
})

test_that("codex responses request targets backend endpoint", {
  token <- fake_codex_jwt("acct_123")
  chat <- chat_openai_codex_responses(
    credentials = openai_codex_credentials(access_token = token),
    model = "gpt-5.5",
    echo = "none"
  )

  req <- chat_request(
    chat$get_provider(),
    stream = TRUE,
    turns = list(UserTurn("Hi"))
  )

  expect_equal(req$url, "https://chatgpt.com/backend-api/codex/responses")
  expect_true("chatgpt-account-id" %in% names(req$headers))
  expect_true("OpenAI-Beta" %in% names(req$headers))
  expect_equal(req$headers[["accept"]], "text/event-stream")
})

test_that("codex body serializes image and PDF content", {
  provider <- chat_openai_codex_responses(
    credentials = openai_codex_credentials(access_token = fake_codex_jwt()),
    echo = "none"
  )$get_provider()

  body <- chat_body(
    provider,
    stream = TRUE,
    turns = list(UserTurn(list(
      ContentText("Describe these."),
      content_image_url("https://example.com/image.png"),
      content_pdf_file(
        if (file.exists("apples.pdf")) {
          "apples.pdf"
        } else {
          "tests/testthat/apples.pdf"
        }
      )
    )))
  )

  types <- vapply(body$input, \(x) x$content[[1]]$type, character(1))
  expect_equal(types, c("input_text", "input_image", "input_file"))
  expect_equal(
    body$input[[2]]$content[[1]]$image_url,
    "https://example.com/image.png"
  )
  expect_match(
    body$input[[3]]$content[[1]]$file_data,
    "^data:application/pdf;base64,"
  )
})

test_that("codex value_turn uses Codex pricing metadata", {
  provider <- chat_openai_codex_responses(
    credentials = openai_codex_credentials(access_token = fake_codex_jwt()),
    model = "gpt-5.5",
    echo = "none"
  )$get_provider()

  turn <- value_turn(
    provider,
    list(
      output = list(list(
        type = "message",
        content = list(list(text = "Hello"))
      )),
      usage = list(
        input_tokens = 100,
        input_tokens_details = list(cached_tokens = 10),
        output_tokens = 20
      ),
      service_tier = "priority"
    )
  )

  # GPT-5.5 priority is 2.5x: ((90 * $5) + (20 * $30) + (10 * $0.50)) / 1e6 * 2.5
  expect_equal(unclass(turn@cost), 0.0026375)
})

test_that("codex stream merges output item events", {
  provider <- chat_openai_codex_responses(
    credentials = openai_codex_credentials(access_token = fake_codex_jwt()),
    echo = "none"
  )$get_provider()

  result <- stream_merge_chunks(
    provider,
    NULL,
    list(
      type = "response.output_item.added",
      output_index = 0,
      item = list(
        id = "fc_1",
        type = "function_call",
        status = "in_progress",
        arguments = "",
        call_id = "call_1",
        name = "add_numbers"
      )
    )
  )
  result <- stream_merge_chunks(
    provider,
    result,
    list(
      type = "response.function_call_arguments.delta",
      output_index = 0,
      delta = "{\"a\":6"
    )
  )
  result <- stream_merge_chunks(
    provider,
    result,
    list(
      type = "response.function_call_arguments.done",
      output_index = 0,
      arguments = "{\"a\":6,\"b\":7}"
    )
  )
  result <- stream_merge_chunks(
    provider,
    result,
    list(
      type = "response.completed",
      response = list(output = list(), usage = NULL)
    )
  )

  expect_equal(length(result$output), 1)
  expect_equal(result$output[[1]]$type, "function_call")
  expect_equal(result$output[[1]]$arguments, "{\"a\":6,\"b\":7}")
})
