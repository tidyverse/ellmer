is_chat <- function(x) {
  inherits(x, "Chat")
}

as_chat <- function(x, error_arg = caller_arg(x), error_call = caller_env()) {
  if (is_chat(x)) {
    x
  } else if (is_string(x)) {
    chat(x)
  } else {
    stop_input_type(
      x,
      c("a string", "a <Chat> object"),
      arg = error_arg,
      call = error_call
    )
  }
}
