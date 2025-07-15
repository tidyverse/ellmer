#' @include tools-def.R
NULL

#' Record and replay content
#'
#' @description
#' These generic functions can be use to convert [Turn]/[Content] objects
#' into easily serializable representations (i.e. lists and atomic vectors).
#'
#' * `contents_record()` accepts a [Turn] or [Content] and return a simple list.
#' * `contents_replay()` takes the output of `contents_record()` and returns
#'   a [Turn] or [Content] object.
#'
#' @param x A [Turn] or [Content] object to serialize; or a serialized object
#'   to replay.
#' @param tools A named list of tools
#' @keywords internal
#' @export
contents_record <- function(x) {
  class_name <- class(x)[[1]]

  if (!grepl("^ellmer::", class_name)) {
    cli::cli_abort(
      "Only S7 classes from the `ellmer` package are currently supported. Received: {.val {class_name}}."
    )
  }

  # Assume that we can replay attributes (the underlying data in an S7 object)
  # to the constructor, i.e. do.call(S7_class(obj), attributes(obj))
  # This avoids inspecting read-only and/or dynamic properties
  attr <- attributes(x)
  serializable_props <- intersect(prop_names(x), names(attr))

  # Don't serialize the actual tool definition, as it really belongs to the
  # Chat object() and on replay we will re-apply by matching to existing tools
  if (class_name == "ellmer::ContentToolRequest") {
    serializable_props <- setdiff(serializable_props, "tool")
  }

  prop_values <- lapply(attr[serializable_props], function(value) {
    if (S7_inherits(value)) {
      # Recursive record for S7 objects
      contents_record(value)
    } else if (is_list_of_s7_objects(value)) {
      # Make record of each item in list
      lapply(value, contents_record)
    } else {
      value
    }
  })

  recorded_object(class_name, prop_values)
}

#' @rdname contents_record
#' @export
contents_replay <- function(x, tools = list()) {
  check_recorded(x)

  class_name <- gsub("^ellmer::", "", x$class)
  cls <- ns_env("ellmer")[[class_name]]
  if (is.null(cls)) {
    cli::cli_abort("Unable to find the S7 class: {.val {x$class}}.")
  }
  if (!S7_inherits(cls)) {
    cli::cli_abort(
      "The object returned for {.val {x$class}} is not an S7 class."
    )
  }

  obj_props <- map(x$props, function(prop_value) {
    if (is_list_of_recorded_objects(prop_value)) {
      # If the prop is a list of recorded objects, replay each one
      map(prop_value, contents_replay, tools = tools)
    } else if (is_recorded_object(prop_value)) {
      # If the prop is a recorded object, replay it
      contents_replay(prop_value, tools = tools)
    } else {
      prop_value
    }
  })

  # This is a bit of overkill, but gives nicer tracebacks
  out <- exec(class_name, !!!obj_props, .env = ns_env("ellmer"))

  if (class_name == "Turn") {
    out <- match_tools(out, tools)
  }
  out
}

# Helpers ----------------------------------------------------------------------

recorded_object <- function(class, props) {
  list(
    version = 1,
    class = class,
    props = props
  )
}

is_recorded_object <- function(x) {
  is.list(x) && all(c("version", "class", "props") %in% names(x))
}

is_list_of_s7_objects <- function(x) {
  is.list(x) && all(map_lgl(x, S7_inherits))
}

is_list_of_recorded_objects <- function(x) {
  is.list(x) && all(map_lgl(x, is_recorded_object))
}

check_recorded <- function(recorded, call = caller_env()) {
  if (!is_recorded_object(recorded)) {
    cli::cli_abort(
      "Expected the recorded object to be a list with at least names 'version', 'class', and 'props'.",
      call = call
    )
  }

  if (!identical(recorded$version, 1)) {
    cli::cli_abort("Unsupported version {.val {recorded$version}}.")
  }

  if (!is_string(recorded$class)) {
    cli::cli_abort(
      "Expected the recorded object to have a single $class name, containing `::` if the class is from a package.",
      call = call
    )
  }

  if (!grepl("ellmer::", recorded$class, fixed = TRUE)) {
    cli::cli_abort(
      "Only S7 classes from the `ellmer` package are currently supported. Received: {.val {recorded$class}}.",
      call = call
    )
  }
}
