#' @include tools-def.R
#' @include as-json.R

ToolBuiltIn <- new_class(
  "ToolBuiltIn",
  properties = list(name = prop_string(), json = class_any)
)

method(as_json, list(Provider, ToolBuiltIn)) <- function(provider, x, ...) {
  x@json
}
