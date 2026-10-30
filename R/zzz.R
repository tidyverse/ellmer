.onLoad <- function(libname, pkgname) {
  run_on_load()
  if ("S7_on_load" %in% getNamespaceExports("S7")) {
    getExportedValue("S7", "S7_on_load")()
  } else {
    S7::methods_register()
  }
  otel_cache_tracer()
}

# Work around S7 bug
rm(format)
rm(print)

# enable usage of <S7_object>@name in package code
#' @rawNamespace if (getRversion() < "4.3.0") importFrom("S7", "@")
NULL

.onUnload <- function(...) {
  if ("S7_on_unload" %in% getNamespaceExports("S7")) {
    getExportedValue("S7", "S7_on_unload")()
  }
}

if ("S7_on_build" %in% getNamespaceExports("S7")) {
  getExportedValue("S7", "S7_on_build")()
}
