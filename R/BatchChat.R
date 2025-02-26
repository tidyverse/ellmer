batch_wait <- function(provider, batch) {
  info <- batch_info(provider, batch)
  cli::cli_progress_bar(
    format = paste0(
      "{cli::pb_spin} ",
      "{info$counts$processing} -> {cli::col_green({info$counts$succeeded})} / {cli::col_red({info$counts$failed})} ",
      "[{cli::pb_elapsed}]"
    ),
    clear = FALSE
  )
  tryCatch({
    while (info$working) {
      for (i in 1:10) {
        Sys.sleep(0.1)
        cli::cli_progress_update()
      }
      batch <- batch_poll(provider, batch)
      info <- batch_info(provider, batch)
    }
  }, interrupt = function(cnd) {})

  batch
}
