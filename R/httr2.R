# Currently performing chat request is not generic as there appears to
# be sufficiently genericity elsewhere to handle the API variations.
# We will recconsider this in the future if necessary.
chat_perform <- function(
  provider,
  mode = c("value", "stream", "async-stream", "async-value"),
  turns,
  tools = list(),
  type = NULL
) {
  mode <- arg_match(mode)
  stream <- mode %in% c("stream", "async-stream")

  req <- chat_request(
    provider = provider,
    turns = turns,
    tools = tools,
    stream = stream,
    type = type
  )

  switch(
    mode,
    "value" = chat_perform_value(provider, req),
    "stream" = chat_perform_stream(provider, req),
    "async-value" = chat_perform_async_value(provider, req),
    "async-stream" = chat_perform_async_stream(provider, req)
  )
}

chat_perform_value <- function(provider, req) {
  resp_body_json(req_perform(req))
}

on_load(
  chat_perform_stream <- coro::generator(function(provider, req) {
    resp <- req_perform_connection(req)
    on.exit(close(resp))

    repeat {
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

chat_perform_async_value <- function(provider, req) {
  promises::then(req_perform_promise(req), resp_body_json)
}

on_load(
  chat_perform_async_stream <- coro::async_generator(function(provider, req) {
    resp <- req_perform_connection(req, blocking = FALSE)
    on.exit(close(resp))

    repeat {
      event <- chat_resp_stream(provider, resp)
      if (is.null(event) && isIncomplete(resp$body)) {
        fds <- curl::multi_fdset(resp$body)
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

ellmer_req_credentials <- function(req, credentials_fun) {
  # TODO: simplify once req_headers_redacted() supports !!!
  credentials <- credentials_fun()
  req_headers(req, !!!credentials, .redact = names(credentials))
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
