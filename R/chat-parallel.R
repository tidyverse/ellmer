parallel_requests <- function(
  provider,
  existing_turns,
  new_turns,
  tools = list(),
  type = NULL,
  rpm = 60
) {
  reqs <- map(new_turns, function(new_turn) {
    chat_request(
      provider = provider,
      turns = c(existing_turns, list(new_turn)),
      tools = tools,
      stream = FALSE,
      type = type
    )}
  )
  reqs <- map(reqs, function(req) {
    req_throttle(req, capacity = rpm, fill_time_s = 60)
  })

  reqs
}
