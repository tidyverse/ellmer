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
#' * `contents_record()` will accept a [Turn] or [Content] related objects and return a
#'   basic list that can be easily serialized.
#' * `contents_replay()` will accept a basic list (from `contents_record()`) and
#'   return a corresponding [Turn] or [Content] related object.
#' * `contents_replay_class()` is a generic function that is dispatched from
#'   within `contents_replay()`. `contents_replay()` will retrieve the
#'   corresponding contructor class from within the basic list information and
#'   use the class for dispatching.
#'
#' Note, all S7 classes should have the same class name as the variable name. Ex: `FooBar <- new_class("FooBar")`, not `OtherName <- new_class("FooBar")`. This is a requirement for when replaying the object.
#'
#' @param content A [Turn] or [Content] object to be recorded.
#' @param obj A basic list (from `contents_record()`) to be replayed.
#' @param cls The class constructor to be used for replaying the object.
#' @param chat A [Chat] object to be used for recording and replaying.
#' @param env The environment to find non-package classes.
#' @param ... Not used.
#'
#' @examples
#' \dontrun{
#' chat <- chat_ollama(model = "llama3.2")
#' turn <- Turn("user")
#' turn
#' #> <Turn: user>
#'
#' # Get the turn record
#' # Note: Removes all S7 class instances
#' turn_recorded <- contents_record(turn, chat = chat)
#' str(turn_recorded)
#' #> List of 3
#' #>  $ version: num 1
#' #>  $ class  : chr "ellmer::Turn"
#' #>  $ props  :List of 4
#' #>   ..$ role    : chr "user"
#' #>   ..$ contents: list()
#' #>   ..$ json    : list()
#' #>   ..$ tokens  : num [1:2] 0 0
#'
#' # Restore the turn from the record
#' # Note: This will not restore the _original_ object,
#' # but a new object with the same properties
#' turn_replayed <- contents_replay(turn_recorded, chat = chat)
#' turn_replayed
#' #> <Turn: user>
#' }
#' @export
#' @rdname contents_record
contents_record <-
  new_generic(
    "contents_record",
    "content",
    function(content, ..., chat) {
      if (!(R6::is.R6(chat) && inherits(chat, "Chat"))) {
        cli::cli_abort(
          "Expected a Chat object at `chat=`, but received {.val {chat}}.",
          call = caller_env()
        )
      }

      recorded <- S7::S7_dispatch()

      for (name in c("version", "class", "props")) {
        if (!name %in% names(recorded)) {
          cli::cli_abort(
            "Expected the recorded object to have a {.val {name}} property.",
            call = caller_env()
          )
        }
      }
      if (
        !is.character(recorded$class) ||
          length(recorded$class) != 1
      ) {
        cli::cli_abort(
          "Expected the recorded object to have a single $class name, containing `::` if the class is from a package.",
          call = caller_env()
        )
      }

      recorded
    }
  )
method(contents_record, S7::S7_object) <- function(content, ..., chat) {
  prop_names <- S7::prop_names(content)
  class_name <- class(content)[1]
  list(
    version = 1,
    class = class(content)[1],
    props = setNames(
      lapply(prop_names, function(prop_name) {
        prop_value <- S7::prop(prop_name, object = content)
        if (S7_inherits(prop_value)) {
          contents_record(prop_value, chat = chat)
        } else {
          prop_value
        }
      }),
      prop_names
    )
  )
}
method(contents_record, Turn) <- function(content, ..., chat) {
  list(
    version = 1,
    class = class(content)[1],
    props = list(
      role = content@role,
      contents = lapply(content@contents, contents_record, chat = chat),
      json = content@json,
      tokens = content@tokens
      # text = getter only!
    )
  )
}

#' @rdname contents_record
#' @export
# Holy "Holy Trait" dispatching, Batman!
contents_replay <- function(
  obj,
  ...,
  cls = NULL,
  chat,
  env = rlang::caller_env()
) {
  if (!(R6::is.R6(chat) && inherits(chat, "Chat"))) {
    cli::cli_abort(
      "Expected a Chat object at `chat=`, but received {.val {chat}}.",
      call = caller_env()
    )
  }

  # Find any reason to not believe `obj` is a recorded object.
  # If not a recorded object, return it as is.
  # If it is a recorded s7 object, dispatch on the discovered class.

  if (!is.list(obj)) {
    return(obj)
  }
  if (!all(c("version", "class", "props") %in% names(obj))) {
    return(obj)
  }

  class_value <- obj$class
  if (!(is.character(class_value) && length(class_value) > 0)) {
    return(obj)
  }

  cls <- get_cls_constructor(class_value[1], env = env)

  if (is.null(cls)) {
    return(obj)
  }

  if (!S7_inherits(cls)) {
    return(obj)
  }

  # Manually retrieve the handler for the class as we dispatch on the class itself,
  # not on an instance
  # An error will be thrown if a method is not found,
  # however we have a fallback for the `S7::S7_object` (the root base class)
  handler <- S7::method(contents_replay_class, cls)
  handler(cls, obj, chat = chat, env = env)
}

#' @rdname contents_record
#' @export
contents_replay_class <- new_generic(
  "contents_replay_class",
  "cls",
  function(cls, obj, ..., chat, env = rlang::caller_env()) {
    S7::S7_dispatch()
  }
)


method(contents_replay_class, S7::S7_object) <- function(
  cls,
  obj,
  ...,
  chat,
  env = rlang::caller_env()
) {
  stopifnot(obj$version == 1)

  obj_props <- lapply(obj$props, contents_replay, chat = chat)

  class_value <- obj$class[1]
  if (grepl("::", class_value, fixed = TRUE)) {
    # If the class is a package class, use the package name to find the constructor
    # This allows use to reach into private namespaces within the package
    pkg_cls <- strsplit(class_value, "::")[[1]]
    pkg_name <- pkg_cls[1]

    rlang::check_installed(
      pkg_name,
      reason = "for `contents_replay()` to restore the chat content."
    )
    cls_name <- pkg_cls[2]
    env <- rlang::pkg_env(pkg_name)
  } else {
    cls_name <- class_value
  }

  # While this seems like a bit of extra work, the tracebacks are accurate
  # vs referencing an unrelated parameter name in the traceback
  rlang::exec(
    cls_name,
    !!!obj_props,
    .env = env
  )
}


method(contents_record, ToolDef) <- function(content, ..., chat) {
  list(
    version = 1,
    class = class(content)[1],
    props = list(
      name = content@name,
      # Do not record the function!
      # It is not serializable and will not be neeeded after replay as the _real_ tool would be leveraged.
      # However, keep all the other properties as the metadata could be useful.
      fun = NULL,
      description = content@description,
      arguments = contents_record(content@arguments, chat = chat),
      convert = content@convert,
      annotations = content@annotations
    )
  )
}
method(contents_replay_class, ToolDef) <- function(
  cls,
  obj,
  ...,
  chat,
  env = rlang::caller_env()
) {
  if (obj$version != 1) {
    cli::cli_abort(
      "Unsupported version {.val {obj$version}}.",
      call = caller_env()
    )
  }

  tools <- chat$get_tools()
  matched_tool <- tools[[obj$props$name]]

  if (!is.null(matched_tool)) {
    return(matched_tool)
  }

  # If no tool is found, return placeholder tool
  ToolDef(
    name = obj$props$name,
    # fun = NULL, # fun was not serialized
    description = obj$props$description,
    arguments = contents_replay(obj$props$arguments, chat = chat),
    convert = obj$props$convert,
    annotations = obj$props$annotations
  )
}


method(contents_record, TypeObject) <- function(content, ..., chat) {
  list(
    version = 1,
    class = class(content)[1],
    props = list(
      description = content@description,
      required = content@required,
      properties = lapply(
        content@properties,
        contents_record,
        chat = chat
      ),
      additional_properties = content@additional_properties
    )
  )
}
method(contents_replay_class, TypeObject) <- function(
  cls,
  obj,
  ...,
  chat,
  env = rlang::caller_env()
) {
  if (obj$version != 1) {
    cli::cli_abort(
      "Unsupported version {.val {obj$version}}.",
      call = caller_env()
    )
  }

  TypeObject(
    description = obj$props$description,
    required = obj$props$required,
    properties = lapply(
      obj$props$properties,
      contents_replay,
      chat = chat
    ),
    additional_properties = obj$props$additional_properties
  )
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
get_cls_constructor <- function(class_value, ..., env = rlang::caller_env()) {
  rlang::check_dots_empty()

  pkg_cls <- strsplit(class_value, "::")[[1]]
  if (length(pkg_cls) == 1) {
    # If the class is not a package class, return the object as is
    # This is the case for local S7 objects
    rlang::eval_bare(rlang::sym(pkg_cls), env = env)
  } else if (length(pkg_cls) == 2) {
    pkg_name <- pkg_cls[1]
    cls_name <- pkg_cls[2]

    rlang::check_installed(
      pkg_name,
      reason = "for `contents_replay()` to restore the chat content."
    )
    rlang::pkg_env(pkg_name)[[cls_name]]
  } else {
    cli::cli_abort(
      "Invalid class name {.val {class_value[1]}}. Expected a single (or missing) `::` separator, not multiple.",
      call = caller_env()
    )
  }
}

expect_record_replay <- function(
  x,
  ...,
  chat = chat_ollama_test("Be as terse as possible; no punctuation"),
  env = rlang::caller_env()
) {
  rlang::check_dots_empty()

  # Simulate the full bookmarking experience:
  # * Record the object to something serializable
  # * Serialize the object to JSON via shiny; "bookmark"
  # * Unserialize the object from JSON via shiny; "restore"
  # * Replay the unserialized object to the original object
  # * Check that the replayed object has the same class as the original object
  # * Check that the replayed object has the same properties as the original object

  obj <- contents_record(x, chat = chat)

  # obj_packed <- jsonlite:::pack(obj)

  # Work around Shiny's terrible JSON serialization
  # Use `as.character()` to remove the JSON class so that it is double serialized :-/
  marshalled <- list(
    "my_chat" = as.character(jsonlite::serializeJSON(obj))
  )

  # Bookmark
  serialized <- shiny:::toJSON(marshalled)
  unserialized <- shiny:::safeFromJSON(serialized)

  # obj_unpacked <- jsonlite:::unpack(unserialized)
  unmarshalled <- jsonlite::unserializeJSON(unserialized$my_chat)

  replayed <- contents_replay(unmarshalled, chat = chat, env = env)

  expect_s3_class(replayed, class(x)[1])
  expect_equal(S7::props(replayed), S7::props(x))

  invisible(replayed)
}
