write_codex_auth <- function(path, access_token) {
  payload <- jsonlite::toJSON(
    list(
      `https://api.openai.com/auth` = list(
        chatgpt_account_is_fedramp = FALSE
      )
    ),
    auto_unbox = TRUE
  )
  payload <- jsonlite::base64_enc(charToRaw(payload))
  payload <- gsub("\n", "", payload, fixed = TRUE)
  payload <- sub("=+$", "", chartr("+/", "-_", payload))

  jsonlite::write_json(
    list(
      auth_mode = "chatgpt",
      tokens = list(
        id_token = paste("e30", payload, "signature", sep = "."),
        access_token = access_token,
        refresh_token = "test-refresh-token",
        account_id = "test-account"
      )
    ),
    file.path(path, "auth.json"),
    auto_unbox = TRUE
  )
}

openai_test_response <- function() {
  response_json(
    body = list(
      id = "resp_test",
      object = "response",
      status = "completed",
      output = list(list(
        id = "msg_test",
        type = "message",
        status = "completed",
        role = "assistant",
        content = list(list(
          type = "output_text",
          text = "ok",
          annotations = list()
        ))
      )),
      usage = list(
        input_tokens = 1L,
        input_tokens_details = list(cached_tokens = 0L),
        output_tokens = 1L,
        total_tokens = 2L
      )
    )
  )
}

test_that("can authenticate with Codex", {
  codex_home <- withr::local_tempdir()
  withr::local_envvar(CODEX_HOME = codex_home)
  write_codex_auth(codex_home, "test-token")

  local_mocked_responses(function(req) {
    headers <- req_get_headers(req, "reveal")
    expect_equal(
      req_get_url(req),
      "https://chatgpt.com/backend-api/codex/responses"
    )
    expect_equal(headers$Authorization, "Bearer test-token")
    expect_equal(headers$`ChatGPT-Account-ID`, "test-account")
    expect_equal(headers$originator, "ellmer")
    openai_test_response()
  })

  chat <- chat_openai(auth = "codex", model = "gpt-4.1-nano", echo = "none")
  expect_equal(as.character(chat$chat("Hi")), "ok")
  expect_equal(as.numeric(chat$last_turn()@cost), 0)
})

test_that("Codex authentication refreshes once after a 401", {
  codex_home <- withr::local_tempdir()
  withr::local_envvar(CODEX_HOME = codex_home)
  write_codex_auth(codex_home, "old-token")

  refreshes <- 0
  local_mocked_bindings(
    codex_refresh = function() {
      refreshes <<- refreshes + 1
      write_codex_auth(codex_home, "new-token")
    }
  )

  tokens <- character()
  local_mocked_responses(function(req) {
    tokens <<- c(tokens, req_get_headers(req, "reveal")$Authorization)
    if (length(tokens) == 1) {
      response_json(401, body = list(error = list(message = "expired")))
    } else {
      openai_test_response()
    }
  })

  chat <- chat_openai(auth = "codex", model = "gpt-4.1-nano", echo = "none")
  expect_equal(as.character(chat$chat("Hi")), "ok")
  expect_equal(tokens, c("Bearer old-token", "Bearer new-token"))
  expect_equal(refreshes, 1)
})

test_that("Codex authentication errors do not reveal credentials", {
  codex_home <- withr::local_tempdir()
  withr::local_envvar(CODEX_HOME = codex_home)

  writeLines(
    '{"tokens":{"access_token":"secret-access-token"',
    file.path(codex_home, "auth.json")
  )
  chat <- chat_openai(auth = "codex", echo = "none")
  error <- rlang::catch_cnd(chat$chat("Hi"))
  expect_match(conditionMessage(error), "Can't read Codex authentication")
  expect_false(grepl(
    "secret-access-token",
    conditionMessage(error),
    fixed = TRUE
  ))

  jsonlite::write_json(
    list(
      auth_mode = "chatgpt",
      tokens = list(
        id_token = "e30.c2VjcmV0LWlkLXRva2Vu.signature",
        access_token = "test-token",
        account_id = "test-account"
      )
    ),
    file.path(codex_home, "auth.json"),
    auto_unbox = TRUE
  )
  error <- rlang::catch_cnd(chat$chat("Hi"))
  expect_match(conditionMessage(error), "Invalid ID token")
  expect_false(grepl("secret-id-token", conditionMessage(error), fixed = TRUE))
})
