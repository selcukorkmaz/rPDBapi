#' Retrieve Specific Fields for Search Results from RCSB PDB
#'
#' This function searches the Protein Data Bank (PDB) for entries related to a specified search term and retrieves
#' specific information from those entries. It is useful for extracting targeted data from search results, such as
#' citations, experimental methods, or structural details. The function leverages `query_search` to perform the
#' initial search and `get_info` to fetch detailed data for each PDB entry.
#'
#' @param search_term A string specifying the term to search for in the PDB. This term can relate to various aspects
#'   of the PDB entries, such as keywords, molecular functions, protein names, or specific research areas.
#' @param field A string indicating the specific field to retrieve for each search result. The default is "citation".
#'   The field should correspond to one of the following valid options:
#'   \describe{
#'     \item{"citation"}{Information about the primary citation associated with the PDB entry.}
#'     \item{"audit_author"}{Details about the authors who contributed to the PDB entry.}
#'     \item{"cell"}{Cell dimensions and related crystallographic information.}
#'     \item{"diffrn"}{Information about the diffraction experiment.}
#'     \item{"diffrn_detector"}{Details about the detector used in the diffraction experiment.}
#'     \item{"diffrn_radiation"}{Radiation source details used in the diffraction experiment.}
#'     \item{"diffrn_source"}{Source of the radiation used in the experiment.}
#'     \item{"entry"}{Basic information about the PDB entry, including its identifier.}
#'     \item{"exptl"}{Details about the experimental methods used to determine the structure.}
#'     \item{"exptl_crystal"}{Information about the crystals used in the experiment.}
#'     \item{"exptl_crystal_grow"}{Details on the crystal growth conditions.}
#'     \item{"pdbx_sgproject"}{Information on the Structural Genomics Project.}
#'     \item{"pdbx_audit_revision_details"}{Details of any revisions made to the PDB entry.}
#'     \item{"pdbx_audit_revision_history"}{History of the revisions for the PDB entry.}
#'     \item{"pdbx_database_related"}{Related database entries.}
#'     \item{"pdbx_database_status"}{Current status of the PDB entry in the database.}
#'     \item{"rcsb_accession_info"}{Accession information for the PDB entry.}
#'     \item{"rcsb_entry_container_identifiers"}{Identifiers associated with the entry container.}
#'     \item{"rcsb_entry_info"}{General information about the PDB entry.}
#'     \item{"rcsb_primary_citation"}{Details of the primary citation for the PDB entry.}
#'     \item{"refine"}{Information about the refinement of the structure.}
#'     \item{"refine_hist"}{History of the refinement process.}
#'     \item{"refine_ls_restr"}{Details about the least-squares restraints used in refinement.}
#'     \item{"reflns"}{Information about the reflections used in the crystallographic experiment.}
#'     \item{"reflns_shell"}{Details about the shell reflections used in the experiment.}
#'     \item{"software"}{Software used in the structure determination process.}
#'     \item{"struct"}{Structural information about the PDB entry.}
#'     \item{"struct_keywords"}{Keywords associated with the structure.}
#'     \item{"symmetry"}{Symmetry information of the crystal structure.}
#'     \item{"rcsb_id"}{The RCSB ID of the PDB entry.}
#'   }
#' @return A named list where each element's name is a PDB ID and its value is the information for the specified field
#'   from the corresponding search result. If no results are found, or if an error occurs during data retrieval, the
#'   function returns an empty list with appropriate warnings or error messages.
#'
#' @details
#' This function is ideal for researchers who need to extract specific data fields from multiple PDB entries efficiently.
#' The process involves two main steps:
#' \enumerate{
#'   \item **Search Query**: The function uses `query_search` to find PDB entries that match the provided search term.
#'   \item **Field Retrieval**: For each PDB ID returned by the search, `get_info` is used to retrieve the specified field.
#' }
#'
#' Error handling is robust, with informative messages provided when the search term yields no results, when an individual
#' PDB entry cannot be retrieved, or when the specified field is not found in the retrieved data.
#'
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET
#' @examples
#' \donttest{
#' # Retrieve citation information for PDB entries related to CRISPR
#' crispr_citations <- find_results("CRISPR", field = "citation")
#' crispr_citations
#' }
#' @export

find_results <- function(search_term, field = "citation") {
  # Retrieve search result IDs with enhanced error handling
  search_result_ids <- tryCatch(
    {
      query_search(search_term)
    },
    error = function(e) {
      stop("Failed to fetch search results for term '", search_term, "'. Error: ", e$message)
    }
  )

  # If search_result_ids is NULL or empty, return an empty list
  if (is.null(search_result_ids) || length(search_result_ids) == 0) {
    warning("No search results found for term '", search_term, "'. Returning an empty list.")
    return(list())
  }

  # Initialize a list to store results
  all_results <- list()

  # Iterate over each ID and fetch the required field with enhanced error handling
  for (i in 1:length(search_result_ids)) {
    pdb_info <- tryCatch(
      {
        get_info(search_result_ids[i])
      },
      error = function(e) {
        warning("Failed to fetch information for PDB ID '", search_result_ids[i], "'. Error: ", e$message)
        return(NULL)
      }
    )

    if (!is.null(pdb_info)) {
      if (field %in% names(pdb_info)) {
        all_results[[search_result_ids[i]]] <- pdb_info[[field]]
      } else {
        warning("Field '", field, "' not found in the information retrieved for PDB ID '", search_result_ids[i], "'.")
      }
    }
  }

  return(all_results)
}


