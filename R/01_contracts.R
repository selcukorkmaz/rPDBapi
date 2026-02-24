# Internal helpers for API contracts and typed errors.

rpdbapi_abort <- function(message, class = "rPDBapi_error", ..., call = NULL) {
  cond <- structure(
    c(list(message = message, call = call), list(...)),
    class = c(class, "rPDBapi_error", "error", "condition")
  )
  stop(cond)
}

rpdbapi_add_class <- function(x, class_name) {
  class(x) <- unique(c(class_name, class(x)))
  x
}
