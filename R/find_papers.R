#' Search for and Retrieve Paper Titles from PDB
#'
#' This function searches the Protein Data Bank (PDB) for scholarly articles related to a specified search term.
#' It retrieves the titles of up to a specified maximum number of papers associated with PDB entries.
#' The function relies on `query_search` to perform the initial search and `get_info` to fetch detailed information
#' for each PDB entry, including the citation titles.
#'
#' @param search_term A string specifying the term to search for in the PDB. This term can relate to any aspect of
#'   the PDB entries, such as keywords, molecular functions, or specific proteins (e.g., "CRISPR").
#' @param max_results An integer indicating the maximum number of paper titles to retrieve. Defaults to 10.
#'   The function will retrieve the titles for the first `max_results` PDB entries returned by the search.
#' @return A named list where each element's name is a PDB ID and its value is the title of the corresponding paper.
#'   If no papers are found or an error occurs, the function returns an empty list with warnings or error messages
#'   to help diagnose the issue.
#'
#' @details
#' This function is useful for researchers who want to quickly find relevant literature associated with specific
#' PDB entries. The process involves two main steps:
#' \enumerate{
#'   \item **Search Query**: The function uses `query_search` to find PDB entries matching the search term.
#'   \item **Fetching Paper Titles**: For each PDB ID returned by the search, `get_info` is used to retrieve
#'   detailed information, including the titles of any associated citations.
#' }
#'
#' The function includes robust error handling to manage cases where the search term does not return any results,
#' or where there are issues retrieving details for specific PDB entries. Warnings are provided if no citations are
#' found for a given PDB ID or if other issues are encountered.
#'
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET
#' @examples
#' \donttest{
#' # Find papers related to CRISPR and retrieve up to 5 paper titles
#' crispr_papers <- find_papers("CRISPR", max_results = 5)
#' print(crispr_papers)
#'
#' }
#' @export

find_papers <- function(search_term, max_results = 10) {
  all_papers <- list()

  # Error handling for the query search
  id_list <- tryCatch(
    {
      query_search(search_term)
    },
    error = function(e) {
      stop("Failed to fetch search results for term '", search_term, "'. Error: ", e$message)
    }
  )

  # If no IDs are found, return an empty list
  if (is.null(id_list) || length(id_list) == 0) {
    warning("No search results found for term '", search_term, "'. Returning an empty list.")
    return(all_papers)
  }

  # Limit the number of results to max_results
  pdbIds <- id_list[1:min(max_results, length(id_list))]

  for (i in seq_along(pdbIds)) {
    # Error handling for fetching individual paper info
    pdb_info <- tryCatch(
      {
        get_info(pdbIds[i])
      },
      error = function(e) {
        warning("Failed to fetch information for PDB ID '", pdbIds[i], "'. Error: ", e$message)
        return(NULL)
      }
    )

    if (!is.null(pdb_info)) {
      if ("citation" %in% names(pdb_info)) {
        all_papers[[pdbIds[i]]] <- pdb_info$citation$title[1]
      } else {
        warning("Citation field not found for PDB ID '", pdbIds[i], "'.")
      }
    }
  }

  return(all_papers)
}

