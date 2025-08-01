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

  check_is_ellmer_object(x)

  # Assume that we can replay attributes (the underlying data in an S7 object)
  # to the constructor, i.e. do.call(S7_class(obj), attributes(obj))
  # This avoids inspecting read-only and/or dynamic properties
  attr <- attributes(x)
  serializable_props <- intersect(prop_names(x), names(attr))

  # Don't serialize the actual tool definition, as it really belongs to the Chat
  # object() and on replay we will re-apply by matching to existing tools. Note
  # that it's not possible for users to extend this class because ellmer always
  # creates the tool request object, so we can check the class directly.
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
#' @param .envir The environment in which to look for class definitions. Used
#'   when the recorded objects include classes that extend [ellmer::Turn] or
#'   [ellmer::Content] but are not from the \pkg{ellmer} package itself.
#' @export
contents_replay <- function(x, tools = list(), .envir = parent.frame()) {
  check_recorded(x)

  class <- recorded_class_info(x, .envir = .envir)

  obj_props <- map(x$props, function(prop_value) {
    if (is_list_of_recorded_objects(prop_value)) {
      # If the prop is a list of recorded objects, replay each one
      map(prop_value, contents_replay, tools = tools, .envir = .envir)
    } else if (is_recorded_object(prop_value)) {
      # If the prop is a recorded object, replay it
      contents_replay(prop_value, tools = tools, .envir = .envir)
    } else {
      prop_value
    }
  })

  env <- if (!is.null(class$pkg)) ns_env(class$pkg) else .envir

  # This is a bit of overkill, but gives nicer tracebacks
  out <- exec(class$name, !!!obj_props, .env = env)

  if (class$name == "Turn") {
    out <- match_tools(out, tools)
  }
  out
}

# Helpers ----------------------------------------------------------------------

check_is_ellmer_object <- function(x) {
  if (S7_inherits(x, ellmer::Content) || S7_inherits(x, ellmer::Turn)) {
    return(invisible(x))
  }

  cli::cli_abort(
    c(
      "Cannot record or replay {.obj_type_friendly {x}}.",
      "i" = "Only {.code ellmer::Content} or {.code ellmer::Turn} classes or subclasses are currently supported."
    ),
    call = caller_env()
  )
}

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
}

recorded_class_info <- function(x, .envir = parent.frame()) {
  class_split <- strsplit(x$class, "::")[[1]]
  if (length(class_split) > 2) {
    cli::cli_abort(
      "Expected the class to be in the form `package::ClassName`, not {.val {x$class}}.",
      call = caller_env()
    )
  }

  if (length(class_split) < 2) {
    pkg <- NULL
    name <- class_split[[1]]
  } else {
    pkg <- class_split[[1]]
    name <- class_split[[2]]
  }

  check_recorded_class(pkg, name, .envir)

  list(pkg = pkg, name = name)
}

check_recorded_class <- function(pkg, name, .envir = parent.frame()) {
  if (is.null(pkg)) {
    cls_fmt <- name
    cls <- get0(name, envir = .envir, inherits = TRUE)
  } else {
    cls_fmt <- sprintf("%s::%s", pkg, name)
    cls <- ns_env(pkg)[[name]]
  }

  if (is.null(cls)) {
    cli::cli_abort(
      "Unable to find the S7 class: {.code {cls_fmt}}.",
      call = caller_env(n = 2)
    )
  }
  if (!S7_inherits(cls)) {
    cli::cli_abort(
      "Expected the object named {.code {cls_fmt}} to be an S7 class, not {.obj_type_friendly {cls}}.",
      call = caller_env(n = 2)
    )
  }

  invisible(cls)
}
