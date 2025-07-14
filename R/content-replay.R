#' @include tools-def.R
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
#' @param chat A [Chat] object to be used for context.
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
contents_record <- new_generic(
  "contents_record",
  "content",
  function(content, ..., chat) {
    check_chat(chat, call = caller_env())

    recorded <- S7_dispatch()

    if (!is_recorded_object(recorded)) {
      cli::cli_abort(
        "Expected the recorded object to be a list with at least names 'version', 'class', and 'props'."
      )
    }

    if (!is.character(recorded$class) || length(recorded$class) != 1) {
      cli::cli_abort(
        "Expected the recorded object to have a single $class name, containing `::` if the class is from a package."
      )
    }

    if (!grepl("ellmer::", recorded$class, fixed = TRUE)) {
      cli::cli_abort(
        "Only S7 classes from the `ellmer` package are currently supported. Received: {.val {recorded$class}}."
      )
    }

    recorded
  }
)

method(contents_record, S7_object) <- function(content, ..., chat) {
  class_name <- class(content)[[1]]

  # Remove read-only props
  cls_props <- S7_class(content)@properties
  prop_names <- names(cls_props)[!map_lgl(cls_props, prop_is_read_only)]

  recorded_props <- setNames(
    lapply(prop_names, function(prop_name) {
      prop_value <- prop(prop_name, object = content)
      if (S7_inherits(prop_value)) {
        # Recursive record for S7 objects
        contents_record(prop_value, chat = chat)
      } else if (is_list_of_s7_objects(prop_value)) {
        # Make record of each item in list
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
contents_replay <- function(obj, ..., chat) {
  check_chat(chat, call = caller_env())

  # Find any reason to not believe `obj` is a recorded object.
  # If not a recorded object, return it as is.
  # If it is a recorded s7 object, dispatch on the discovered class.

  if (!is_recorded_object(obj)) {
    cli::cli_abort(
      "Expected the object to be a list with at least names 'version', 'class', and 'props'."
    )
  }

  class_name <- obj$class
  if (!(is.character(class_name) && length(class_name) == 1)) {
    cli::cli_abort(
      "Expected the replay object's `'class'` value to be a single character."
    )
  }

  cls_name <- strsplit(class_name, "::")[[1]][2]
  if (!grepl("ellmer::", class_name, fixed = TRUE)) {
    cli::cli_abort(
      "Only S7 classes from the `ellmer` package are currently supported."
    )
  }

  cls <- pkg_env("ellmer")[[cls_name]]

  if (is.null(cls)) {
    cli::cli_abort("Unable to find the S7 class: {.val {class_name}}.")
  }

  if (!S7_inherits(cls)) {
    cli::cli_abort(
      "The object returned for {.val {class_name}} is not an S7 class."
    )
  }

  # Manually retrieve the handler for the class as we dispatch on the class itself,
  # not on an instance
  # An error will be thrown if a method is not found,
  # however we have a fallback for the `S7_object` (the root base class)
  handler <- method(contents_replay_class, cls)
  handler(cls, obj, chat = chat)
}

contents_replay_class <- new_generic(
  "contents_replay_class",
  "cls",
  function(cls, obj, ..., chat) {
    S7_dispatch()
  }
)


method(contents_replay_class, S7_object) <- function(cls, obj, ..., chat) {
  stopifnot(obj$version == 1)

  obj_props <- map(obj$props, function(prop_value) {
    if (is_list_of_recorded_objects(prop_value)) {
      # If the prop is a list of recorded objects, replay each one
      map(prop_value, contents_replay, chat = chat)
    } else if (is_recorded_object(prop_value)) {
      # If the prop is a recorded object, replay it
      contents_replay(prop_value, chat = chat)
    } else {
      prop_value
    }
  })

  class_name <- obj$class[1]
  cls_name <- strsplit(class_name, "::")[[1]][2]
  # While this seems like a bit of extra work, the tracebacks are accurate
  # vs referencing an unrelated parameter name in the traceback
  exec(cls_name, !!!obj_props, .env = ns_env("ellmer"))
}

method(contents_replay_class, ToolDef) <- function(cls, obj, ..., chat) {
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
    super(cls, S7_object),
    obj,
    chat = chat
  )
  ret
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
