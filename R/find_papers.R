#' Search for and Retrieve Paper Titles from PDB
#'
#' This function searches for papers in the Protein Data Bank (PDB) using a specified search term.
#' It retrieves the titles of papers up to a specified maximum number of results.
#' The function assumes the presence of `query_search` and `get_info` functions to perform the search and fetch paper details.
#'
#' @param search_term A string specifying the term to search for in the PDB.
#' @param max_results An integer indicating the maximum number of paper titles to retrieve.
#'   Defaults to 10.
#' @return A named list where each element's name is a PDB ID and its value is the title of the corresponding paper.
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET
#' @export
find_papers <- function(search_term, max_results = 10) {
  # Assuming Query and get_info functions are already defined in R

  all_papers <- list()
  id_list <- query_search(search_term)[[1]]

  pdbIds = id_list[1:max_results]

  for (i in 1:length(pdbIds)) {
    pdb_info <- get_info(pdbIds[i])
    if ("citation" %in% names(pdb_info)) {

      all_papers[[pdbIds[i]]] <- pdb_info$citation$title[1]
    }
  }

  return((all_papers))
}

