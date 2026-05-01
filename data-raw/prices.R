if (interactive() || !nzchar(Sys.getenv("CI"))) {
  system2("bash", "data-raw/prices.sh")
}

prices_data <- jsonlite::fromJSON("data-raw/prices.json")
usethis::use_data(prices_data, overwrite = TRUE, internal = TRUE)
