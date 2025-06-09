#' @include utils-S7.R
#' @include turns.R
#' @include tools-def.R
#' @include content.R

NULL

#' Save and restore content
#'
#' @description
#' These generic functions can be use to convert [Turn]/[Content] objects
#' into easily serializable representations.
#'
#' * `contents_record()` accept a [Turn] or [Content] and return a simple list.
#' * `contents_replay()` will accept a simple list (from `contents_record()`)
#'   and return a [Turn] or [Content] object.
#'
#' @param content A [Turn] or [Content] object to serialize.
#' @param obj A basic list to desierialize.
#' @param cls The class constructor to be used for replaying the object.
#' @param chat A [Chat] object to be used for context.
#' @param env The environment to find non-package classes.
#' @param ... Not used.
#'
#' @examplesIf has_credentials("openai")
#' chat <- chat_openai(model = "gpt-4.1-nano")
#' chat$chat("Where is the capital of France?")
#'
#' # Serialize to a simple list
#' turn_recorded <- contents_record(chat$get_turns(), chat = chat)
#' str(turn_recorded)
#'
#' # Deserialize back to S7 objects
#' turn_replayed <- contents_replay(turn_recorded, chat = chat)
#' turn_replayed
#' @export
#' @rdname contents_record
contents_record <-
  new_generic(
    "contents_record",
    "content",
    function(content, ..., chat) {
      check_chat(chat, call = caller_env())

      if (is_list_of_s7_objects(content)) {
        # If the content is a list, we need to record each element
        # and return a list of the recorded elements
        return(lapply(content, contents_record, chat = chat))
      }

      recorded <- S7::S7_dispatch()

      if (!is_recorded_object(recorded)) {
        cli::cli_abort(
          "Expected the recorded object to be a list with at least names 'version', 'class', and 'props'."
        )
      }

      if (
        !is.character(recorded$class) ||
          length(recorded$class) != 1
      ) {
        cli::cli_abort(
          "Expected the recorded object to have a single $class name, containing `::` if the class is from a package."
        )
      }

      recorded
    }
  )

method(contents_record, S7::S7_object) <- function(content, ..., chat) {
  class_name <- class(content)[1]

  # Remove read-only props
  cls_props <- S7::S7_class(content)@properties
  prop_names <- names(cls_props)[!map_lgl(cls_props, prop_is_read_only)]

  recorded_props <- setNames(
    lapply(prop_names, function(prop_name) {
      prop_value <- S7::prop(prop_name, object = content)
      if (S7_inherits(prop_value)) {
        # Recursive call to S7 object
        contents_record(prop_value, chat = chat)
      } else if (is_list_of_s7_objects(prop_value)) {
        # Make record of each item in prop.
        # Do not recurse forever!
        lapply(prop_value, contents_record, chat = chat)
      } else {
        prop_value
      }
    }),
    prop_names
  )

  # Remove non-serializable properties
  recorded_props <- Filter(function(x) !is.function(x), recorded_props)

  list(
    version = 1,
    class = class_name,
    props = recorded_props
  )
}


#' @rdname contents_record
#' @export
# Holy "Holy Trait" dispatching, Batman!
contents_replay <- function(obj, ..., chat, env = caller_env()) {
  check_chat(chat, call = caller_env())

  # Find any reason to not believe `obj` is a recorded object.
  # If not a recorded object, return it as is.
  # If it is a recorded s7 object, dispatch on the discovered class.

  if (!is.list(obj)) {
    return(obj)
  }

  if (!is_recorded_object(obj)) {
    if (is_list_of_recorded_objects(obj)) {
      return(lapply(obj, contents_replay, chat = chat, env = env))
    }
    return(obj)
  }

  class_value <- obj$class
  if (!(is.character(class_value) && length(class_value) > 0)) {
    return(obj)
  }

  cls <- get_cls_constructor(class_value[1], env = env)
  # Manually retrieve the handler for the class as we dispatch on the class itself,
  # not on an instance
  # An error will be thrown if a method is not found,
  # however we have a fallback for the `S7::S7_object` (the root base class)
  handler <- S7::method(contents_replay_class, cls)
  handler(cls, obj, chat = chat, env = env)
}

contents_replay_class <- new_generic(
  "contents_replay_class",
  "cls",
  function(cls, obj, ..., chat, env = caller_env()) {
    S7::S7_dispatch()
  }
)


method(contents_replay_class, S7::S7_object) <- function(
  cls,
  obj,
  ...,
  chat,
  env = caller_env()
) {
  stopifnot(obj$version == 1)

  obj_props <- lapply(obj$props, contents_replay, chat = chat)

  class_value <- obj$class[1]
  if (grepl("::", class_value, fixed = TRUE)) {
    # If the class is a package class, use the package name to find the constructor
    # This allows use to reach into private namespaces within the package
    pkg_cls <- strsplit(class_value, "::")[[1]]
    pkg_name <- pkg_cls[1]

    check_installed(
      pkg_name,
      reason = "for `contents_replay()` to restore the chat content."
    )
    cls_name <- pkg_cls[2]
    env <- ns_env(pkg_name)
  } else {
    cls_name <- class_value
  }

  # While this seems like a bit of extra work, the tracebacks are accurate
  # vs referencing an unrelated parameter name in the traceback
  exec(cls_name, !!!obj_props, .env = env)
}


method(contents_replay_class, ToolDef) <- function(
  cls,
  obj,
  ...,
  chat,
  env = caller_env()
) {
  if (obj$version != 1) {
    cli::cli_abort(
      "Unsupported version {.val {obj$version}}."
    )
  }

  tools <- chat$get_tools()
  matched_tool <- tools[[obj$props$name]]

  if (!is.null(matched_tool)) {
    return(matched_tool)
  }

  # If no tool is found, return placeholder tool containing the metadata
  ret <- contents_replay_class(
    super(cls, S7::S7_object),
    obj,
    chat = chat,
    env = env
  )
  ret
}


#' Retrieve the class constructor
#'
#' @description
#' The class for S7 Classes are stored as "package::class" in the recorded object.
#' However, it does not mean that the class constructor is exported in the namespace.
#' Therefore, this function will reach into the package environment to retrieve the constructor.
#' If the class is not a _package_ class, it will return the object as is given the `env`.
#'
#' @param class_value The single string representing the `package::class` to retrieve the constructor.
#' @param ... Not used.
#' @param env The environment to find non-package class constructors.
#' @return The constructor function for the class.
#' @noRd
get_cls_constructor <- function(class_value, ..., env = caller_env()) {
  check_dots_empty()

  pkg_cls <- strsplit(class_value, "::")[[1]]
  if (length(pkg_cls) == 1) {
    # If the class is not a package class, return the object as is
    # This is the case for local S7 objects
    cls <- eval_bare(sym(pkg_cls), env = env)
  } else if (length(pkg_cls) == 2) {
    pkg_name <- pkg_cls[1]
    cls_name <- pkg_cls[2]

    check_installed(
      pkg_name,
      reason = "for `contents_replay()` to restore the chat content."
    )
    cls <- ns_env(pkg_name)[[cls_name]]
  } else {
    cli::cli_abort(
      "Invalid class name {.val {class_value[1]}}. Expected a single (or missing) `::` separator, not multiple."
    )
  }
  if (is.null(cls)) {
    cli::cli_abort("Unable to find the S7 class: {.val {class_value[1]}}.")
  }

  if (!S7_inherits(cls)) {
    cli::cli_abort(
      "The object returned for {.val {class_value[1]}} is not an S7 class."
    )
  }
  cls
}

prop_is_read_only <- function(prop) {
  is.function(prop$getter) && !is.function(prop$setter)
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
