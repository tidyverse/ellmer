#' Describe the schema of a data frame, suitable for sending to an LLM
#'
#' @description
#' `df_schema()` gives a column-by-column description of a data frame. For
#' each column, it gives the name, type, label (if present), and number of
#' missing values. For numeric and date/time columns, it also gives the
#' range. For character and factor columns, it also gives the number of unique
#' values, and if there's only a few (<= 10), their values.
#'
#' The goal is to give the LLM a sense of the structure of the data, so that
#' it can generate useful code, and the output attempts to balance between
#' conciseness and accuracy.
#'
#' @param df A data frame to describe.
#' @param max_cols Maximum number of columns to includes. Defaults to 50 to
#'   avoid accidentally generating very large prompts.
#' @export
#' @examples
#' df_schema(mtcars)
#' df_schema(iris)
df_schema <- function(df, max_cols = 50) {
  if (!is.data.frame(df)) {
    stop_input_type(df, "a data frame")
  }

  df_desc <- sprintf(
    "A data frame with %i rows and %i columns:",
    nrow(df),
    ncol(df)
  )

  if (ncol(df) > max_cols) {
    cli::cli_warn("Truncating to {max_cols} columns.")
    df <- df[seq_len(max_cols)]
    extra <- sprintf("and %i more columns", ncol(df) - max_cols)
  } else {
    extra <- NULL
  }

  cols <- map_chr(df, col_schema)
  col_desc <- paste0("* ", names(cols), ": ", cols, recycle0 = TRUE)

  desc <- paste0(c(df_desc, col_desc, extra), collapse = "\n")
  ellmer_prompt(desc)
}

col_schema <- new_generic("col_schema", "x")

method(col_schema, class_logical) <- function(x) {
  describe_column(
    "logical",
    desc_label(x),
    sprintf("%i TRUEs", sum(x, na.rm = TRUE)),
    sprintf("%i FALSEs", sum(!x, na.rm = TRUE)),
    desc_na(x)
  )
}

method(col_schema, class_numeric) <- function(x) {
  describe_column(
    if (is.integer(x)) "integer" else "numeric",
    desc_label(x),
    desc_range(x),
    desc_na(x)
  )
}

method(col_schema, class_character) <- function(x) {
  describe_column(
    "character",
    desc_label(x),
    desc_na(x),
    desc_unique(x)
  )
}

method(col_schema, class_factor) <- function(x) {
  describe_column(
    if (is.ordered(x)) "ordinal" else "nominal",
    desc_label(x),
    desc_na(x),
    desc_unique(x)
  )
}

method(col_schema, class_Date) <- function(x) {
  describe_column(
    "date",
    desc_label(x),
    desc_range(x),
    desc_na(x)
  )
}

method(col_schema, class_POSIXt) <- function(x) {
  tz <- attr(x, "tzone")

  describe_column(
    "date-time",
    if (!is.null(tz) && tz != "") sprintf("timezone %s", tz),
    desc_label(x),
    desc_range(x),
    desc_na(x)
  )
}

method(col_schema, class_data.frame) <- function(x) {
  describe_column(
    "data frame",
    paste(names(x), collapse = ", "),
    desc_na(x)
  )
}

method(col_schema, class_list) <- function(x) {
  describe_column("list column")
}

method(col_schema, class_any) <- function(x) {
  describe_column(paste0(class(x), collapse = "/"))
}


# Helpers ----------------------------------------------------------------------

describe_column <- function(type, ...) {
  if (missing(...)) {
    ellmer_prompt(type)
  } else {
    props <- c(...)
    ellmer_prompt(paste0(type, " with ", str_flatten(props)))
  }
}

desc_range <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return()
  }
  rng <- range(x)
  sprintf(
    "range [%s, %s]",
    format(rng[1], digits = 4),
    format(rng[2], digits = 4)
  )
}

desc_unique <- function(x) {
  if (is.factor(x)) {
    label <- "permitted"
    unique <- levels(x)
  } else {
    label <- "unique"
    unique <- unique(x)
  }
  unique <- unique[!is.na(unique)]

  desc <- sprintf("%i %s values", length(unique), label)
  if (length(unique) > 0 && length(unique) <= 10 && sum(nchar(unique)) < 200) {
    quoted <- encodeString(unique, quote = '"')
    desc <- paste0(desc, " (", str_flatten(quoted, last = NULL), ")")
  }
  desc
}

desc_na <- function(x) {
  sprintf("%i NAs", sum(is.na(x)))
}

desc_label <- function(x) {
  label <- attr(x, "label")
  if (is.null(label)) {
    return()
  }
  sprintf("label (%s)", label)
}


str_flatten <- function(string, collapse = ", ", last = ", and ") {
  n <- length(string)
  if (!is.null(last) && n >= 2) {
    string <- c(
      string[seq2(1, n - 2)],
      paste0(string[[n - 1]], last, string[[n]])
    )
  }

  paste0(string, collapse = collapse)
}
