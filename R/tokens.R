on_load(
  the$tokens <- tokens_row()
)

tokens <- function(input = 0, output = 0, cached_input = 0) {
  check_number_whole(input, allow_null = TRUE)
  check_number_whole(output, allow_null = TRUE)
  check_number_whole(cached_input, allow_null = TRUE)

  list(
    input = input %||% 0,
    output = output %||% 0,
    cached_input = cached_input %||% 0
  )
}

tokens_log <- function(provider, tokens, variant = "") {
  i <- vctrs::vec_match(
    data.frame(
      provider = provider@name,
      model = provider@model,
      variant = variant
    ),
    the$tokens[c("provider", "model", "variant")]
  )

  if (is.na(i)) {
    new_row <- tokens_row(
      provider@name,
      provider@model,
      variant,
      tokens$input,
      tokens$output,
      tokens$cached_input
    )
    the$tokens <- rbind(the$tokens, new_row)
  } else {
    the$tokens$input[i] <- the$tokens$input[i] + tokens$input
    the$tokens$output[i] <- the$tokens$output[i] + tokens$output
    the$tokens$cached_input[i] <- the$tokens$cached_input[i] +
      tokens$cached_input
  }

  invisible()
}

tokens_row <- function(
  provider = character(0),
  model = character(0),
  variant = character(0),
  input = numeric(0),
  output = numeric(0),
  cached_input = numeric(0)
) {
  data.frame(
    provider = provider,
    model = model,
    variant = variant,
    input = input,
    output = output,
    cached_input = cached_input
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
    out$variant,
    out$input,
    out$output,
    out$cached_input
  )
  out
}

# Cost ----------------------------------------------------------------------

has_cost <- function(provider, model) {
  needle <- data.frame(provider = provider@name, model = model)
  vctrs::vec_in(needle, prices[c("provider", "model")])
}

get_token_cost <- function(
  provider,
  model,
  variant,
  input = 0,
  output = 0,
  cached_input = 0
) {
  needle <- data.frame(provider = provider, model = model, variant = variant)
  idx <- vctrs::vec_match(needle, prices[c("provider", "model", "variant")])

  if (any(is.na(idx))) {
    # Match baseline if we can't match specific variant
    no_match <- is.na(idx)
    needle$variant <- ""

    idx[no_match] <- vctrs::vec_match(
      needle[no_match],
      prices[c("provider", "model", "variant")]
    )
  }

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
