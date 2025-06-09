test_record_replay <- function(
  x,
  ...,
  chat = chat_openai_test(),
  env = rlang::caller_env()
) {
  rlang::check_dots_empty()

  shiny__to_json <- rlang::ns_env("shiny")[["toJSON"]]
  shiny__safe_from_json <- rlang::ns_env("shiny")[["safeFromJSON"]]

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
  serialized <- shiny__to_json(marshalled)
  unserialized <- shiny__safe_from_json(serialized)

  # obj_unpacked <- jsonlite:::unpack(unserialized)
  unmarshalled <- jsonlite::unserializeJSON(unserialized$my_chat)

  replayed <- contents_replay(unmarshalled, chat = chat, env = env)

  expect_s3_class(replayed, class(x)[1])
  expect_equal(S7::props(replayed), S7::props(x))

  invisible(replayed)
}
