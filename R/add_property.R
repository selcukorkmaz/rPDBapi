#' Add or Merge Properties for RCSB PDB Data Fetching
#'
#' This function is designed for handling properties related to fetching data from the Protein Data Bank (PDB).
#' It takes a dictionary, where keys represent properties and values are lists of subproperties.
#' If a property already exists in the input list, the function merges the subproperties,
#' ensuring each subproperty is unique and maintains character vector format.
#'
#' @param property A Python dictionary where keys are the properties (like 'cell', 'exptl') and
#' values are lists of subproperties (like 'volume', 'angle_beta', 'method').
#' Each subproperty should be in character vector format.
#' @return A modified list with updated properties where subproperties are merged if a property already exists.
#' @examples
#' properties <- list(cell = c("volume", "angle_beta"), exptl = c("method"))
#' add_property(properties)
#' @export

add_property <- function(property) {
  if (!is.list(property)) {
    stop("Property must be a list.")
  }

  for (key in names(property)) {
    if (!is.character(property[[key]])) {
      stop("Property values must be character vectors.")
    }
    if (!key %in% names(property)) {
      property[[key]] <- character()
    }
    property[[key]] <- unique(c(property[[key]], property[[key]]))
  }

  return(property)
}
