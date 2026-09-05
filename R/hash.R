# Simple stable hash for objects used in batch state
hash <- function(x) {
  json <- jsonlite::toJSON(x, auto_unbox = TRUE, null = "null")
  as.character(openssl::md5(json))
}

