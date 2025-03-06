MockedChat <- R6::R6Class(
  "MockedChat",
  inherit = Chat,
  public = list(
    i = 0,
    saved_chats = character(),

    initialize = function(saved_chats) {
      self$saved_chats <- saved_chats
    },

    chat = function(...) {
      self$i <- self$i + 1
      self$saved_chats[self$i]
    }
  )
)

mocked_chat <- function(chats) {
  MockedChat$new(saved_chats = chats)
}
