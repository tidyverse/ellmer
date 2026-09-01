# Currently performing chat request is not generic as there appears to
# be sufficiently genericity elsewhere to handle the API variations.
# We will recconsider this in the future if necessary.
chat_perform <- function(
  provider,
  model,
  mode = c("value", "stream", "async-stream", "async-value"),
  turns,
  tools = NULL,
  type = NULL,
  otel_span = NULL,
  controller = NULL
) {
  mode <- arg_match(mode)
  stream <- mode %in% c("stream", "async-stream")
  tools <- tools %||% list()

  setup_active_promise_otel_span(otel_span)

  req <- chat_request(
    provider = provider,
    model = model,
    turns = turns,
    tools = tools,
    stream = stream,
    type = type
  )
  req <- ellmer_req_connect_viewer(req)

  switch(
    mode,
    "value" = req_perform(req),
    "stream" = chat_perform_stream(
      provider,
      req,
      controller = controller,
      otel_span = otel_span
    ),
    "async-value" = req_perform_promise(req),
    "async-stream" = chat_perform_async_stream(
      provider,
      req,
      controller = controller,
      otel_span = otel_span
    )
  )
}

on_load(
  chat_perform_stream <- coro::generator(function(
    provider,
    req,
    controller = NULL,
    otel_span = NULL
  ) {
    setup_active_promise_otel_span(otel_span)

    resp <- req_perform_connection(req)
    on.exit(close(resp))

    repeat {
      if (!is.null(controller) && controller$cancelled) {
        break
      }
      event <- chat_resp_stream(provider, resp)
      data <- stream_parse(provider, event)
      if (is.null(data)) {
        break
      } else {
        yield(data)
      }
    }
  })
)

on_load(
  chat_perform_async_stream <- coro::async_generator(function(
    provider,
    req,
    controller = NULL,
    otel_span = NULL
  ) {
    setup_active_promise_otel_span(otel_span)

    resp <- req_perform_connection(req, blocking = FALSE)
    on.exit(close(resp))

    repeat {
      if (!is.null(controller) && controller$cancelled) {
        break
      }
      event <- chat_resp_stream(provider, resp)
      if (is.null(event) && !resp_stream_is_complete(resp)) {
        fds <- resp$body$get_fdset()
        await(promises::promise(function(resolve, reject) {
          later::later_fd(
            resolve,
            fds$reads,
            fds$writes,
            fds$exceptions,
            fds$timeout
          )
        }))
        next
      }

      data <- stream_parse(provider, event)
      if (is.null(data)) {
        break
      } else {
        yield(data)
      }
    }
  })
)

# Request helpers --------------------------------------------------------------

ellmer_req_robustify <- function(req, is_transient = NULL, after = NULL) {
  req <- req_timeout(req, getOption("ellmer_timeout_s", 5 * 60))

  req <- req_retry(
    req,
    max_tries = getOption("ellmer_max_tries", 3),
    is_transient = is_transient,
    after = after,
    retry_on_failure = TRUE
  )

  req
}

# When content runs on Posit Connect and its LLM traffic is routed through
# Connect's gateway, forward the current viewer's session token so Connect
# can attribute the call to the viewer who caused it
ellmer_req_connect_viewer <- function(req) {
  if (!is_connect_gateway_url(req$url)) {
    return(req)
  }
  token <- connect_session_token()
  if (is.null(token)) {
    return(req)
  }
  req_headers_redacted(req, `Posit-Connect-User-Session-Token` = token)
}

# The token is only ever sent back to the Connect server that injected it.
is_connect_gateway_url <- function(url) {
  server <- Sys.getenv("CONNECT_SERVER")
  if (identical(server, "")) {
    return(FALSE)
  }
  server <- url_parse(server)
  url <- url_parse(url)
  prefix <- sub("/+$", "", server$path %||% "")
  identical(tolower(url$scheme), tolower(server$scheme)) &&
    identical(tolower(url$hostname), tolower(server$hostname)) &&
    identical(url$port, server$port) &&
    startsWith(url$path %||% "", paste0(prefix, "/__gateway__/"))
}

connect_session_token <- function() {
  # Only read the session token when actually running on Connect
  if (!running_on_connect() || !is_installed("shiny")) {
    return(NULL)
  }
  session <- shiny::getDefaultReactiveDomain()
  session$request$HTTP_POSIT_CONNECT_USER_SESSION_TOKEN
}

running_on_connect <- function() {
  identical(Sys.getenv("RSTUDIO_PRODUCT"), "CONNECT")
}

ellmer_req_user_agent <- function(req, override = "") {
  ua <- if (identical(override, "")) ellmer_user_agent() else override
  req_user_agent(req, ua)
}
ellmer_user_agent <- function() {
  paste0("r-ellmer/", utils::packageVersion("ellmer"))
}
transform_user_agent <- function(x) {
  gsub(ellmer_user_agent(), "<ellmer_user_agent>", x, fixed = TRUE)
}
