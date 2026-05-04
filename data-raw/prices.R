if (interactive() || !nzchar(Sys.getenv("CI"))) {
  system2("bash", "data-raw/prices.sh")
}

prices <- jsonlite::fromJSON("data-raw/prices.json")
usethis::use_data(prices, overwrite = TRUE, internal = TRUE)
