on_load(
  the$tokens <- tokens_row()
)

tokens <- function(input = 0, output = 0, cached_input = 0) {
  check_number_decimal(input, allow_null = TRUE)
  check_number_decimal(output, allow_null = TRUE)
  check_number_decimal(cached_input, allow_null = TRUE)

  list(
    input = input %||% 0,
    output = output %||% 0,
    cached_input = cached_input %||% 0
  )
}

map_tokens <- function(x, f, ...) {
  out <- t(vapply(x, f, double(3)))
  colnames(out) <- c("input", "output", "cached_input")
  out
}

log_tokens <- function(provider, tokens, cost) {
  i <- vctrs::vec_match(
    data.frame(
      provider = provider@name,
      model = provider@model
    ),
    the$tokens[c("provider", "model")]
  )

  if (is.na(i)) {
    new_row <- tokens_row(
      provider@name,
      provider@model,
      tokens$input,
      tokens$output,
      tokens$cached_input,
      cost
    )
    the$tokens <- rbind(the$tokens, new_row)
  } else {
    the$tokens$input[i] <- the$tokens$input[i] + tokens$input
    the$tokens$output[i] <- the$tokens$output[i] + tokens$output
    the$tokens$cached_input[i] <- the$tokens$cached_input[i] +
      tokens$cached_input
    the$tokens$price[i] <- the$tokens$price[i] + cost
  }

  invisible()
}

log_turn <- function(provider, turn) {
  log_tokens(provider, exec(tokens, !!!as.list(turn@tokens)), turn@cost)
}

log_turns <- function(provider, turns) {
  for (turn in turns) {
    if (S7_inherits(turn, AssistantTurn)) {
      log_turn(provider, turn)
    }
  }
}


tokens_row <- function(
  provider = character(0),
  model = character(0),
  input = numeric(0),
  output = numeric(0),
  cached_input = numeric(0),
  price = numeric(0)
) {
  data.frame(
    provider = provider,
    model = model,
    input = input,
    output = output,
    cached_input = cached_input,
    price = price
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

  the$tokens
}

# Cost ----------------------------------------------------------------------

has_cost <- function(provider, model) {
  needle <- data.frame(provider = provider@name, model = model)
  vctrs::vec_in(needle, prices[c("provider", "model")])
}

get_token_cost <- function(provider, tokens, variant = "") {
  needle <- data.frame(
    provider = provider@name,
    model = provider@model,
    variant = variant
  )
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

  input_price <- tokens$input * prices$input[idx] / 1e6
  output_price <- tokens$output * prices$output[idx] / 1e6
  cached_input_price <- tokens$cached_input * prices$cached_input[idx] / 1e6

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
#' @export
`[.ellmer_dollars` <- function(x, ...) {
  dollars(NextMethod())
}
#' @export
`[[.ellmer_dollars` <- function(x, ...) {
  dollars(NextMethod())
}
#' @export
Summary.ellmer_dollars <- function(x, ...) {
  dollars(NextMethod())
}
