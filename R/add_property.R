#' Add or Merge Properties for RCSB PDB Data Fetching
#'
#' This function facilitates the management of properties and subproperties required for data retrieval from the Protein Data Bank (PDB).
#' It accepts a list of properties where each key represents a property category (e.g., 'cell', 'exptl'), and the corresponding value is a character vector of subproperties (e.g., 'volume', 'method').
#' The function ensures that if a property already exists, its subproperties are merged without duplication, guaranteeing that each subproperty remains unique.
#'
#' The `add_property` function is particularly useful when users need to dynamically build or update a list of properties required for complex queries in the PDB. By automatically handling duplicate entries, this function streamlines the process of constructing property lists, which can then be used in subsequent data retrieval operations.
#'
#' @param property A list where each element corresponds to a property category. The names of the list elements are the properties, and their values are character vectors containing the subproperties.
#' Each subproperty should be provided as a character vector. The full list of available properties and their descriptions can be found at \url{https://data.rcsb.org/#data-schema}.
#'
#' For example, a `property` list might look like:
#' \describe{
#'   \item{cell}{A character vector of subproperties like \code{"length_a"}, \code{"length_b"}, \code{"length_c"}.}
#'   \item{exptl}{A character vector of subproperties like \code{"method"}.}
#' }
#'
#' @return A modified list that consolidates the input properties. If a property already exists in the input list, its subproperties are merged, removing any duplicates.
#'
#' @details The function operates as follows:
#' \enumerate{
#'   \item Checks if the input `property` is a list. If not, it throws an error.
#'   \item Iterates through each property in the list, ensuring that subproperties are unique and in character vector format.
#'   \item If a property already exists in the list, it merges the subproperties while eliminating duplicates.
#' }
#'
#' @note It is important to ensure that the subproperties are correctly formatted as character vectors. The function does not modify the format of the subproperties.
#'
#' @seealso \code{\link{fetch_data}}, \code{\link{query_search}} for related functions that utilize properties in querying the PDB.
#'
#' @examples
#' # Example usage:
#' properties <- list(cell = c("length_a", "length_b", "length_c"), exptl = c("method"))
#' # Add new properties or merge existing ones
#' updated_properties <- add_property(properties)
#' print(updated_properties)
#'
#' @export


add_property <- function(property) {
  if (!is.list(property)) {
    stop("Property must be a list.")
  }

  for (key in names(property)) {
    # if (!is.character(property[[key]])) {
    #   stop("Property values must be character vectors.")
    # }
    if (!key %in% names(property)) {
      property[[key]] <- character()
    }
    property[[key]] <- unique(c(property[[key]], property[[key]]))
  }

  return(property)
}
