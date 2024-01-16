#' Retrieve Specific Fields for Search Results from RCSB PDB
#'
#' This function performs a search in the Protein Data Bank (PDB) using a provided search term
#' and retrieves information for a specified field (e.g., citation) for each search result.
#' It relies on `query_search` and `get_info` functions for searching and retrieving detailed information.
#'
#' @param search_term A string specifying the term to search for in the PDB.
#' @param field A string indicating the specific field to retrieve for each search result.
#'   Default is "citation".
#' @return A named list where each element's name is a PDB ID and its value is the information
#'   for the specified field from the corresponding search result.
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET
#' @export
find_results <- function(search_term, field = "citation") {

  # Retrieve search result IDs
  search_result_ids <- query_search(search_term)[[1]]

  # Initialize a list to store results
  all_results <- list()

  # Iterate over each ID and fetch the required field
  for (i in 1:length(search_result_ids)) {
    pdb_info <- get_info(search_result_ids[i])
    if (field %in% names(pdb_info)) {
      all_results[[search_result_ids[i]]] <- pdb_info[[field]]
    }
  }

  return(all_results)
}


