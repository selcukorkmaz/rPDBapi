# Additive object model helpers for richer downstream workflows.

rpdbapi_new_object <- function(data, class_name, metadata = list()) {
  obj <- list(
    data = data,
    metadata = metadata
  )
  class(obj) <- c(class_name, "rPDBapi_object", "list")
  obj
}

rpdbapi_to_tibble <- function(data) {
  if (inherits(data, "data.frame")) {
    return(dplyr::as_tibble(data))
  }

  if (is.list(data)) {
    flat <- lapply(data, function(x) {
      if (is.list(x)) {
        paste(as.character(unlist(x, recursive = TRUE, use.names = FALSE)), collapse = ";")
      } else {
        as.character(x)
      }
    })
    return(dplyr::as_tibble(as.data.frame(flat, stringsAsFactors = FALSE)))
  }

  dplyr::as_tibble(data.frame(value = as.character(data), stringsAsFactors = FALSE))
}

#' Convert Data to an rPDBapi Entry Object
#'
#' @param x Input data.
#' @param metadata Optional metadata list.
#' @return Object of class \code{rPDBapi_entry}.
#' @export
as_rpdb_entry <- function(x, metadata = list()) {
  rpdbapi_new_object(data = x, class_name = "rPDBapi_entry", metadata = metadata)
}

#' Convert Data to an rPDBapi Assembly Object
#'
#' @param x Input data.
#' @param metadata Optional metadata list.
#' @return Object of class \code{rPDBapi_assembly}.
#' @export
as_rpdb_assembly <- function(x, metadata = list()) {
  rpdbapi_new_object(data = x, class_name = "rPDBapi_assembly", metadata = metadata)
}

#' Convert Data to an rPDBapi Polymer Entity Object
#'
#' @param x Input data.
#' @param metadata Optional metadata list.
#' @return Object of class \code{rPDBapi_polymer_entity}.
#' @export
as_rpdb_polymer_entity <- function(x, metadata = list()) {
  rpdbapi_new_object(data = x, class_name = "rPDBapi_polymer_entity", metadata = metadata)
}

#' Convert Data to an rPDBapi Chemical Component Object
#'
#' @param x Input data.
#' @param metadata Optional metadata list.
#' @return Object of class \code{rPDBapi_chemical_component}.
#' @export
as_rpdb_chemical_component <- function(x, metadata = list()) {
  rpdbapi_new_object(data = x, class_name = "rPDBapi_chemical_component", metadata = metadata)
}

#' Convert Data to an rPDBapi Structure Object
#'
#' @param x Input data.
#' @param metadata Optional metadata list.
#' @return Object of class \code{rPDBapi_structure}.
#' @export
as_rpdb_structure <- function(x, metadata = list()) {
  rpdbapi_new_object(data = x, class_name = "rPDBapi_structure", metadata = metadata)
}

#' @export
print.rPDBapi_object <- function(x, ...) {
  cat("<", class(x)[1], ">", " with data class: ", paste(class(x$data), collapse = "/"), "\n", sep = "")
  invisible(x)
}

#' @export
as_tibble.rPDBapi_entry <- function(x, ...) {
  rpdbapi_to_tibble(x$data)
}

#' @export
as_tibble.rPDBapi_assembly <- function(x, ...) {
  rpdbapi_to_tibble(x$data)
}

#' @export
as_tibble.rPDBapi_polymer_entity <- function(x, ...) {
  rpdbapi_to_tibble(x$data)
}

#' @export
as_tibble.rPDBapi_chemical_component <- function(x, ...) {
  rpdbapi_to_tibble(x$data)
}

#' @export
as_tibble.rPDBapi_structure <- function(x, ...) {
  rpdbapi_to_tibble(x$data)
}
