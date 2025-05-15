#' @include utils-S7.R
#' @include turns.R
#' @include tools-def.R
#' @include content.R

NULL

#' Save and restore content
#'
#' @description
#' These generic functions can be use to convert [Turn] contents or [Content]
#' objects into easily serializable representations.
#'
#' * `contents_replay()` will accept a basic object and return a corresponding
#'   [Turn] or [Content] object.
#' * `contents_record()` will accept a [Turn] or [Content] object and return a
#'   basic object that can be easily serialized.
#' @export
contents_record <- new_generic("contents_record", "content")
# #' @export
# contents_record_prop_names <- new_generic(
#   "contents_record_prop_names",
#   "content"
# )

# contents_replay_impl <- new_generic(
#   "contents_replay_impl",
#   "type",
#   function(state, type) {
#     S7::S7_dispatch()
#   }
# )

# method(contents_record, Turn) <- function(content) {
#   list(
#     version = 1,
#     type = "turn",
#     props = list(
#       role = content@role,
#       contents = contents_record(content@contents),
#       tokens = content@tokens,
#       completed = content@completed
#     )
#   )
# }
method(contents_record, S7::S7_object) <- function(content) {
  prop_names <- S7::prop_names(content)
  list(
    version = 1,
    class = class(content)[1],
    props = setNames(
      lapply(prop_names, function(prop_name) {
        prop_value <- S7::prop(prop_name, object = content)
        if (S7_inherits(prop_value)) {
          contents_record(prop_value)
        } else {
          prop_value
        }
      }),
      prop_names
    )
  )
}
method(contents_record, Turn) <- function(content) {
  list(
    version = 1,
    class = class(content)[1],
    props = list(
      role = content@role,
      contents = lapply(content@contents, contents_record),
      json = content@json,
      tokens = content@tokens,
      completed = content@completed
    )
  )
}

#' @rdname contents_record
#' @export
contents_replay <- function(obj) {
  # Find any reason to not believe `obj` is a recorded object.
  # If not a recorded object, return it as is.
  # If it is a recorded s7 object, dispatch on the discovered class.

  if (!is.list(obj)) {
    return(obj)
    # cli::cli_abort(
    #   "Expected a list, but got {.val {obj}}.",
    #   call = caller_env()
    # )
  }
  if (!all(c("version", "class", "props") %in% names(obj))) {
    return(obj)
    # cli::cli_abort(
    #   "Expected a list with version, class, and props keys, but got {.val {obj}}.",
    #   call = caller_env()
    # )
  }
  class_value <- obj$class
  if (!(is.character(class_value) && length(class_value) > 0)) {
    return(obj)
    # cli::cli_abort(
    #   "class key must be a string. {.val {class_value}}.",
    #   call = caller_env()
    # )
  }
  pkg_cls <- strsplit(class_value[1], "::")[[1]]
  if (length(pkg_cls) != 2) {
    return(obj)
    # cli::cli_abort(
    #   "class key must be a string with a package name. {.val {class_value}}.",
    #   call = caller_env()
    # )
  }
  pkg_name <- pkg_cls[1]
  cls_name <- pkg_cls[2]

  cls <- rlang::pkg_env(pkg_name)[[cls_name]]

  if (is.null(cls)) {
    return(obj)
    # cli::cli_abort(
    #   "class key must be a valid class name. {.val {class_value}}.",
    #   call = caller_env()
    # )
  }

  if (!S7_inherits(cls)) {
    return(obj)
    # cli::cli_abort(
    #   "class key must be a S7 class. {.val {class_value}}.",
    #   call = caller_env()
    # )
  }

  # Manually retrieve the handler for the class as we dispatch on the class itself,
  # not on an instance
  # An error will be thrown if a method is not found,
  # however we have a fallback for the `S7::S7_object` (the root base class)
  handler <- S7::method(contents_replay_s7, cls)
  handler(cls, obj)
}

#' @rdname contents_record
#' @export
contents_replay_s7 <- new_generic(
  "contents_replay_s7",
  "cls",
  function(cls, obj) {
    S7::S7_dispatch()
  }
)


method(contents_replay_s7, S7::S7_object) <- function(cls, obj) {
  stopifnot(obj$version == 1)

  obj_props <- lapply(obj$props, contents_replay)
  ## While this should give prettier tracebacks, it doesn't work
  # > cls_name <- rlang::sym(obj$class[1])
  # > rlang::inject((!!cls_name)(!!!obj_props))
  # Error in `ellmer::Turn`(role = "user", contents = list(), json = list(),  :
  # could not find function "ellmer::Turn"

  print(cls)

  rlang::inject(cls(!!!obj_props))
}

with_chat_env <- list2env(list())
with_chat_set <- function(chat) {
  if (is.null(chat)) {
    with_chat_env$chat <- NULL
    return()
  }
  if (!inherits(chat, "Chat")) {
    cli::cli_abort(
      "Expected a Chat object, but got {.val {chat}}.",
      call = caller_env()
    )
  }
  with_chat_env$chat <- chat
}
with_chat_get <- function() {
  chat <- with_chat_env$chat
  if (is.null(chat)) {
    cli::cli_abort(
      "No Chat object found in the environment.",
      call = caller_env()
    )
  }
  chat
}
with_chat <- function(chat, code) {
  with_chat_set(chat)
  on.exit(with_chat_set(NULL), add = TRUE)
  force(code)
}

method(contents_record, ToolDef) <- function(content) {
  list(
    version = 1,
    class = class(content)[1],
    props = list(
      name = content@name
      # description = content$description,
      # arguments = content$arguments,
      # annotations = content$annotations
    )
  )
}
method(contents_replay_s7, ToolDef) <- function(cls, obj) {
  if (obj$version != 1) {
    cli::cli_abort(
      "Unsupported version {.val {obj$version}}.",
      call = caller_env()
    )
  }
  chat <- with_chat_get()
  tools <- chat$get_tools()

  tool <- tools[[obj$props$name]]
  # Return matched tool or NULL
  return(tool)
}

tool_rnorm <- tool(
  stats::rnorm,
  "Drawn numbers from a random normal distribution",
  n = type_integer("The number of observations. Must be a positive integer."),
  mean = type_number("The mean value of the distribution."),
  sd = type_number(
    "The standard deviation of the distribution. Must be a non-negative number."
  )
)

# method(contents_replay_impl, "S7") <- function(state) {
#   if (!is.list(state)) {
#     cli::cli_abort(
#       "Expected a list, but got {.val {state}}.",
#       call = caller_env()
#     )
#   }

#   contents <- lapply(prop_names, function(prop_name) {
#     prop_value <- S7::prop(prop_name, object = content)
#     contents_replay(prop_value)
#   })
#   contents <- unlist(contents, recursive = FALSE)
#   contents <- contents[!sapply(contents, is.null)]
#   contents <- contents[contents != ""]
#   contents
# }

# contents_list_replay <- function(state) {
# }

# contents_record_old <- new_generic("contents_record_old", "content")
# method(contents_record_old_prop_names, S7_object) <- function(content) {
#   S7::prop_names(content)
# }

# method(contents_record_old, S7_object) <- function(content) {
#   prop_names <- contents_record_old_prop_names(content)
#   if (length(prop_names) == 0) {
#     return(NULL)
#   }
#   props <- lapply(prop_names, function(prop_name) {
#     prop_value <- S7::prop(prop_name, object = content)
#     contents_record_old(prop_value)
#   })
#   props <- setNames(props, prop_names)
#   list(
#     version = 1,
#     type = "s7",
#     class = class(content),
#     props = props
#   )
# }

# method(contents_record_old, S7::class_list) <- function(content) {
#   lapply(content, contents_record_old)
# }
# method(contents_record_old, S7::class_any) <- function(content) {
#   content
# }

# contents_replay_s7_cls <- function(state) {
#   if (is.null(state)) {
#     return(NULL)
#   }
#   if (state$version != 1) {
#     cli::cli_abort(
#       "Unsupported version {.val {state$version}}.",
#       call = caller_env()
#     )
#   }

#   if (!identical(state$type, "s7")) {
#     cli::cli_abort(
#       "Unsupported type {.val {state$type}}.",
#       call = caller_env()
#     )
#   }

#   class_value <- state$class
#   if (is.null(class_value)) {
#     cli::cli_abort(
#       "class key must be provided. {.val {class_value}}."
#     )
#   }
#   if (!is.character(class_value)) {
#     cli::cli_abort(
#       "class key must be a string. {.val {class_value}}."
#     )
#   }
#   if (length(class_value) == 0) {
#     cli::cli_abort(
#       "class key must be a single string. {.val {class_value}}."
#     )
#   }
#   if (nchar(class_value[0]) == 0) {
#     cli::cli_abort(
#       "class key must be a non-empty string. {.val {class_value}}."
#     )
#   }

#   s7_cls_info <- strsplit(class_value[0], "::")[[1]]
#   s7_cls_pkg <- s7_cls_info[1]
#   s7_cls_name <- s7_cls_info[2]
#   s7_cls <- rlang::pkg_env(s7_cls_pkg)[[s7_cls_name]]

#   contents_replay(s7_cls, state)
#   # s7_cls
# }

# method(contents_replay, S7::class_list) <- function(state) {
#   lapply(state, contents_replay)
# }
# method(contents_replay, S7::class_any) <- function(cls, state) {
#   state
# }

# method(contents_replay, S7_object) <- function(state, type) {
#   prop_names <- contents_record_prop_names(content)
#   if (length(prop_names) == 0) {
#     return(NULL)
#   }

#   props <- lapply(prop_names, function(prop_name) {
#     prop_value <- S7::prop(prop_name, object = content)
#     contents_replay(prop_value)
#   })

#   rlang::inject(cls(!!!state$props))
# }

# method(contents_replay, Turn) <- function(object) {
#   if (object$version != 1) {
#     cli::cli_abort(
#       "Unsupported version {.val {object$version}}.",
#       call = caller_env()
#     )
#   }
#   if (object$type != "turn") {
#     cli::cli_abort(
#       "Unsupported type {.val {object$type}}.",
#       call = caller_env()
#     )
#   }
#   contents <- lapply(content@contents, contents_replay)
#   contents <- unlist(contents, recursive = FALSE)
#   contents <- contents[!sapply(contents, is.null)]
#   contents <- contents[contents != ""]
#   contents
# }
# method(contents_replay, S7_object) <- function(content) {
#   contents <- lapply(prop_names, function(prop_name) {
#     prop_value <- S7::prop(prop_name, object = content)
#     contents_replay(prop_value)
#   })
#   contents <- unlist(contents, recursive = FALSE)
#   contents <- contents[!sapply(contents, is.null)]
#   contents <- contents[contents != ""]
#   contents
# }
