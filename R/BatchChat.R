# Make print method call `$run()`?

BatchChat <- R6::R6Class(
  "BatchChat",
  public = list(

    # Need to take existing turns and then add user turns to them
    # TODO: optionally automatically cache all previous the turns
    initialize = function(id, provider, turns, type = NULL) {
      private$provider <- provider
      private$turns <- turns
      private$type <- type
    },

    run = function() {
      if (status == "new") {
        private$submitted <- batch_submit(
          private$provider,
          turns = private$turns,
          type = private$type
        )
        private$poll()
      }

      while (status != "completed") {
        # TODO: update progress spinner
        tryCatch(
          {
            result <- batch_poll(private$provider, private$submitted)
            private$polled <- result$body
          },
          interrupt = function(cnd) {
            cli::cli_inform(c(
              x = "Interrupted by user.",
              i = "Use `$run()` to resume."
            ))
            break
          }
        )

        if (result$done) {
          break
        }
      }

      if (status == "completed") {
        cli::cli_inform(c(
          v = "Completed",
          i = "Use `$results()` to get results."
        ))
      }
    },

    results = function() {
      batch_results(private$provider, private$polled)
    }
  ),
  private = list(
    id = NULL,
    provider = NULL,
    turns = NULL,
    type = NULL,

    # "new" | "submitted" | "completed"
    status = NULL,

    submitted = NULL,
    polled = NULL
  )
)
