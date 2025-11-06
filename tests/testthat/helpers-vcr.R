#

vcr_clean <- function(url_prefix) {
  vcr_dirs <- file.path(c("inst", "tests/testthat", "vignettes"), "_vcr")
  cassettes <- dir(vcr_dirs, full.names = TRUE)

  first_uri <- map_chr(cassettes, \(path) {
    yaml <- utils::getFromNamespace("read_yaml", "yaml")(path)
    yaml$http_interactions[[1]]$request$uri
  })
  match <- startsWith(first_uri, url_prefix)
  unlink(cassettes[match])
}

vcr_rebuild <- function() {
  withr::local_temp_libpaths()

  cli::cli_rule("Installing package to temporary location")
  # Needed because build_articles() renders in clean session
  utils::getFromNamespace("install", "devtools")(
    quick = TRUE,
    upgrade = FALSE,
    quiet = TRUE
  )
  utils::getFromNamespace("build_articles", "pkgdown")(lazy = FALSE)
  utils::getFromNamespace("build_reference", "pkgdown")(lazy = FALSE)
  utils::getFromNamespace("test", "devtools")()
}
