batch_wait <- function(provider, batch) {
  info <- batch_info(provider, batch)
  cli::cli_progress_bar(
    format = paste0(
      "{cli::pb_spin} Processing... {info$counts$processing} -> {cli::col_green({info$counts$succeeded})} / {cli::col_red({info$counts$failed})} ",
      "[{cli::pb_elapsed}]"
    ),
    clear = FALSE
  )
  tryCatch(
    {
      while (info$working) {
        Sys.sleep(1)
        cli::cli_progress_update()
        batch <- batch_poll(provider, batch)
        info <- batch_info(provider, batch)
      }
    },
    interrupt = function(cnd) {}
  )

  batch
}


check_has_batch_support <- function(provider, call = caller_env()) {
  if (has_batch_support(provider)) {
    return()
  }

  cli::cli_abort(
    "Batch requests are not currently supported by this provider.",
    call = call
  )
}
