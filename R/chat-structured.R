extract_data <- function(turn, type, convert = TRUE, needs_wrapper = FALSE) {
  is_json <- map_lgl(turn@contents, S7_inherits, ContentJson)
  n <- sum(is_json)
  if (n != 1) {
    cli::cli_abort("Data extraction failed: {n} data results recieved.")
  }

  json <- turn@contents[[which(is_json)]]
  out <- json@value

  if (needs_wrapper) {
    out <- out$wrapper
    type <- type@properties[[1]]
  }
  if (convert) {
    out <- convert_from_type(out, type)
  }
  out
}

wrap_type_if_needed <- function(type, needs_wrapper = FALSE) {
  if (needs_wrapper) {
    type_object(wrapper = type)
  } else {
    type
  }
}

convert_from_type <- function(x, type) {
  if (is.null(x) && !type@required) {
    x
  } else if (S7_inherits(type, TypeArray)) {
    if (S7_inherits(type@items, TypeBasic)) {
      if (!type@items@required) {
        is_null <- map_lgl(x, is.null)
        if (type@items@type == "string") {
          x[is_null] <- list(NA_character_)
        } else {
          x[is_null] <- list(NA)
        }
      }

      switch(
        type@items@type,
        boolean = as.logical(x),
        integer = as.integer(x),
        number = as.numeric(x),
        string = as.character(x),
        cli::cli_abort("Unknown type {type@items@type}", .internal = TRUE)
      )
    } else if (S7_inherits(type@items, TypeArray)) {
      lapply(x, function(y) convert_from_type(y, type@items))
    } else if (S7_inherits(type@items, TypeEnum)) {
      factor(as.character(x), levels = type@items@values)
    } else if (S7_inherits(type@items, TypeObject)) {
      cols <- lapply(names(type@items@properties), function(name) {
        vals <- lapply(x, function(y) y[[name]])
        convert_from_type(
          vals,
          type_array(items = type@items@properties[[name]])
        )
      })
      names(cols) <- names(type@items@properties)
      list2DF(cols)
    } else {
      x
    }
  } else if (S7_inherits(type, TypeObject)) {
    out <- lapply(names(type@properties), function(name) {
      convert_from_type(x[[name]], type@properties[[name]])
    })
    set_names(out, names(type@properties))
  } else if (S7_inherits(type, TypeBasic)) {
    if (is.null(x)) {
      switch(
        type@type,
        boolean = NA,
        integer = NA_integer_,
        number = NA_real_,
        string = NA_character_,
        cli::cli_abort("Unknown type {type@type}", .internal = TRUE)
      )
    } else {
      x
    }
  } else {
    x
  }
}
