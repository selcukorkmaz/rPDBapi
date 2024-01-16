#' Create a Structure Operator for Structure-Based Searches
#'
#' Constructs a StructureOperator object for use in structure-based searches within the RCSB PDB.
#' This operator allows specifying a PDB entry ID, assembly ID, and the search mode.
#'
#' @param pdb_entry_id A string representing the PDB entry ID to search for.
#' @param assembly_id An integer representing the assembly ID, default is 1.
#' @param search_mode A string indicating the search mode, such as 'STRICT_SHAPE_MATCH', default is 'STRICT_SHAPE_MATCH'.
#' @return An object of class 'StructureOperator' representing the structure search operator.
#' @export
StructureOperator <- function(pdb_entry_id, assembly_id = 1, search_mode = "STRICT_SHAPE_MATCH") {

  res <- list(
    value = list(entry_id = pdb_entry_id, assembly_id = as.character(assembly_id)),
    operator = StructureSearchMode[[search_mode]]
  )

  structure(res, class = c("StructureOperator", class(res)))

}

