on_load(
  the$tokens <- tokens_row(character(), character(), numeric(), numeric())
)

tokens_log <- function(provider, input = NULL, output = NULL) {
  input <- input %||% 0
  output <- output %||% 0

  model <- standardise_model(provider, provider@model)

  name <- function(provider, model) paste0(provider, "/", model)
  i <- tokens_match(provider@name, model, the$tokens$provider, the$tokens$model)

  if (is.na(i)) {
    new_row <- tokens_row(provider@name, model, input, output)
    the$tokens <- rbind(the$tokens, new_row)
  } else {
    the$tokens$input[i] <- the$tokens$input[i] + input
    the$tokens$output[i] <- the$tokens$output[i] + output
  }

  # Returns value to be passed to Turn
  c(input, output)
}

tokens_row <- function(provider, model, input, output) {
  data.frame(provider = provider, model = model, input = input, output = output)
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
  the$tokens <- tokens_row(character(), character(), numeric(), numeric())

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
  out$price <- get_token_cost(out$provider, out$model, out$input, out$output)
  out
}

# Cost ----------------------------------------------------------------------

get_token_cost <- function(provider, model, input, output) {
  idx <- tokens_match(provider, model, prices$provider, prices$model)

  input_price <- input * prices$input[idx] / 1e6
  output_price <- output * prices$output[idx] / 1e6
  dollars(input_price + output_price)
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
