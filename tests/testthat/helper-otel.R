with_ellmer_otel_record <- function(...) {
  stopifnot(is_testing())

  # Reset cached tracer now and when `with` scope ends
  reset_tracer <- environment(ellmer_otel_tracer)$reset_tracer
  defer({
    reset_tracer()
  })

  otelsdk::with_otel_record(...)
}
