on_load(
  the$tokens <- tokens_row()
)

tokens_log <- function(
  provider,
  input = NULL,
  output = NULL,
  cached_input = NULL
) {
  input <- input %||% 0
  output <- output %||% 0
  cached_input <- cached_input %||% 0

  i <- tokens_match(
    provider@name,
    provider@model,
    the$tokens$provider,
    the$tokens$model
  )

  if (is.na(i)) {
    new_row <- tokens_row(
      provider@name,
      provider@model,
      input,
      output,
      cached_input
    )
    the$tokens <- rbind(the$tokens, new_row)
  } else {
    the$tokens$input[i] <- the$tokens$input[i] + input
    the$tokens$output[i] <- the$tokens$output[i] + output
    the$tokens$cached_input[i] <- the$tokens$cached_input[i] + cached_input
  }

  # Returns value to be passed to Turn
  c(input, output, cached_input)
}

tokens_row <- function(
  provider = character(0),
  model = character(0),
  input = numeric(0),
  output = numeric(0),
  cached_input = numeric(0)
) {
  data.frame(
    provider = provider,
    model = model,
    input = input,
    output = output,
    cached_input = cached_input
  )
}

tokens_match <- function(
  provider_needle,
  model_needle,
  provider_haystack,
  model_haystack
) {
  match(
    paste0(provider_needle, "/", model_needle),
    paste0(provider_haystack, "/", model_haystack)
  )
}


local_tokens <- function(frame = parent.frame()) {
  old <- the$tokens
  the$tokens <- tokens_row()

  defer(the$tokens <- old, env = frame)
}

#' Report on token usage in the current session
#'
#' Call this function to find out the cumulative number of tokens that you
#' have sent and recieved in the current session. The price will be shown
#' if known.
#'
#' @export
#' @return A data frame
#' @examples
#' token_usage()
token_usage <- function() {
  if (nrow(the$tokens) == 0) {
    cli::cli_inform(c(x = "No recorded usage in this session"))
    return(invisible(the$tokens))
  }

  out <- the$tokens
  out$price <- get_token_cost(
    out$provider,
    out$model,
    out$input,
    out$output,
    out$cached_input
  )
  out
}

# Cost ----------------------------------------------------------------------

has_cost <- function(provider, model) {
  !is.na(tokens_match(provider@name, model, prices$provider, prices$model))
}

get_token_cost <- function(
  provider,
  model,
  input,
  output,
  cached_input
) {
  idx <- tokens_match(provider, model, prices$provider, prices$model)

  input_price <- input * prices$input[idx] / 1e6
  output_price <- output * prices$output[idx] / 1e6
  cached_input_price <- cached_input * prices$cached_input[idx] / 1e6

  dollars(input_price + output_price + cached_input_price)
}

dollars <- function(x) {
  structure(x, class = c("ellmer_dollars", "numeric"))
}
#' @export
format.ellmer_dollars <- function(x, ...) {
  paste0(ifelse(is.na(x), "", "$"), format(unclass(round(x, 2)), nsmall = 2))
}
#' @export
print.ellmer_dollars <- function(x, ...) {
  print(format(x), quote = FALSE)
  invisible(x)
}
