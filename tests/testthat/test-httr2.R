test_that("viewer token is forwarded only to Connect's gateway", {
  withr::local_envvar(CONNECT_SERVER = "https://connect.example.com/")
  local_mocked_bindings(connect_session_token = \() "token")

  req <- request("https://connect.example.com/__gateway__/anthropic/guid/v1")
  req <- ellmer_req_connect_viewer(req)
  headers <- req_get_headers(req, "reveal")
  expect_equal(headers$`Posit-Connect-User-Session-Token`, "token")

  req <- request("https://api.anthropic.com/v1")
  req <- ellmer_req_connect_viewer(req)
  expect_equal(req_get_headers(req, "reveal"), list())

  req <- request("https://evil.example.com/__gateway__/anthropic/guid/v1")
  req <- ellmer_req_connect_viewer(req)
  expect_equal(req_get_headers(req, "reveal"), list())

  # A host that only shares a string prefix with the server is not the server.
  req <- request("https://connect.example.com.evil.com/__gateway__/x")
  req <- ellmer_req_connect_viewer(req)
  expect_equal(req_get_headers(req, "reveal"), list())
})

test_that("viewer token is forwarded when Connect is on a sub-path", {
  withr::local_envvar(
    CONNECT_SERVER = "https://proxy.example.com/positconnect/"
  )
  local_mocked_bindings(connect_session_token = \() "token")

  req <- request(
    "https://proxy.example.com/positconnect/__gateway__/anthropic/guid/v1"
  )
  req <- ellmer_req_connect_viewer(req)
  headers <- req_get_headers(req, "reveal")
  expect_equal(headers$`Posit-Connect-User-Session-Token`, "token")

  req <- request("https://proxy.example.com/__gateway__/anthropic/guid/v1")
  req <- ellmer_req_connect_viewer(req)
  expect_equal(req_get_headers(req, "reveal"), list())
})

test_that("viewer token is never sent off Connect", {
  withr::local_envvar(CONNECT_SERVER = NA)
  local_mocked_bindings(connect_session_token = \() "token")

  req <- request("https://connect.example.com/__gateway__/anthropic/guid/v1")
  req <- ellmer_req_connect_viewer(req)
  expect_equal(req_get_headers(req, "reveal"), list())
})

test_that("viewer token is redacted from the request", {
  withr::local_envvar(CONNECT_SERVER = "https://connect.example.com")
  local_mocked_bindings(connect_session_token = \() "token")

  req <- request("https://connect.example.com/__gateway__/anthropic/guid/v1")
  req <- ellmer_req_connect_viewer(req)
  expect_equal(
    req_get_headers(req, "redact")$`Posit-Connect-User-Session-Token`,
    "<REDACTED>"
  )
  expect_equal(
    req_get_headers(req, "reveal")$`Posit-Connect-User-Session-Token`,
    "token"
  )
})

test_that("viewer token is read from the shiny session", {
  skip_if_not_installed("shiny")
  withr::local_envvar(
    CONNECT_SERVER = "https://connect.example.com",
    RSTUDIO_PRODUCT = "CONNECT"
  )

  domain <- list(
    request = list(HTTP_POSIT_CONNECT_USER_SESSION_TOKEN = "shiny-token")
  )
  shiny::withReactiveDomain(domain, {
    req <- request("https://connect.example.com/__gateway__/anthropic/guid/v1")
    req <- ellmer_req_connect_viewer(req)
    headers <- req_get_headers(req, "reveal")
    expect_equal(headers$`Posit-Connect-User-Session-Token`, "shiny-token")
  })
})

test_that("session token is not read when not running on Connect", {
  skip_if_not_installed("shiny")
  withr::local_envvar(
    CONNECT_SERVER = "https://connect.example.com",
    RSTUDIO_PRODUCT = NA
  )

  domain <- list(
    request = list(HTTP_POSIT_CONNECT_USER_SESSION_TOKEN = "shiny-token")
  )
  shiny::withReactiveDomain(domain, {
    req <- request("https://connect.example.com/__gateway__/anthropic/guid/v1")
    req <- ellmer_req_connect_viewer(req)
    expect_equal(req_get_headers(req, "reveal"), list())
  })
})

test_that("request is unchanged without a session", {
  withr::local_envvar(CONNECT_SERVER = "https://connect.example.com")

  req <- request("https://connect.example.com/__gateway__/anthropic/guid/v1")
  req <- ellmer_req_connect_viewer(req)
  expect_equal(req_get_headers(req, "reveal"), list())
})
