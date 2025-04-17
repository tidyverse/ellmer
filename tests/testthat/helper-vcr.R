local_cassette_test <- function(name, ..., .frame = parent.frame()) {
  dir.create(test_path("_vcr"), showWarnings = FALSE)

  old <- vcr::vcr_configure(dir = test_path("_vcr"))
  withr::defer(vcr::vcr_configure(!!!old), envir = .frame)

  vcr::local_cassette(name, ..., frame = .frame)
}
