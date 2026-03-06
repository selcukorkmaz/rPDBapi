# Internal helpers for API contracts and typed errors.

rpdbapi_abort <- function(message, class = "rPDBapi_error", ..., call = NULL) {
  cond <- structure(
    c(list(message = message, call = call), list(...)),
    class = c(class, "rPDBapi_error", "error", "condition")
  )
  stop(cond)
}

rpdbapi_rethrow <- function(error, message_prefix = "", class = "rPDBapi_error_request_failed", wrap_typed = FALSE, ..., call = NULL) {
  if (inherits(error, "rPDBapi_error") && !isTRUE(wrap_typed)) {
    stop(error)
  }

  rpdbapi_abort(
    paste0(message_prefix, conditionMessage(error)),
    class = class,
    parent_message = conditionMessage(error),
    ...,
    call = call
  )
}

rpdbapi_add_class <- function(x, class_name) {
  class(x) <- unique(c(class_name, class(x)))
  x
}
