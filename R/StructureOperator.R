#' Create a Structure Operator for Structure-Based Searches
#'
#' The `StructureOperator` function constructs an operator object for conducting structure-based searches within the RCSB Protein Data Bank (PDB).
#' This operator allows users to specify a PDB entry ID, an assembly ID, and the mode of search to be used, facilitating precise structural queries.
#'
#' @param pdb_entry_id A string representing the PDB entry ID to search for. The PDB entry ID is a unique identifier for each structure in the PDB.
#' @param assembly_id An integer representing the assembly ID within the PDB entry. The assembly ID identifies the specific biological assembly or model within the PDB entry.
#'   By default, this is set to 1, which typically corresponds to the first biological assembly or model.
#' @param search_mode A string indicating the search mode to be applied during the structure-based search. Accepted values include 'STRICT_SHAPE_MATCH', 'RELAXED_SHAPE_MATCH', etc.
#'   The default is 'STRICT_SHAPE_MATCH', which ensures a precise comparison based on the structural shape of the molecules.
#' @return An object of class `StructureOperator` that encapsulates the criteria for performing structure-based searches in the RCSB PDB.
#' @examples
#' # Example of creating a structure operator for a specific PDB entry and assembly
#' struct_operator <- StructureOperator(
#'   pdb_entry_id = "1XYZ",
#'   assembly_id = 1,
#'   search_mode = "STRICT_SHAPE_MATCH"
#' )
#' print(struct_operator)
#'
#' # Example of creating a structure operator with a relaxed search mode
#' struct_operator_relaxed <- StructureOperator(
#'   pdb_entry_id = "1ABC",
#'   assembly_id = 2,
#'   search_mode = "RELAXED_SHAPE_MATCH"
#' )
#' print(struct_operator_relaxed)
#' @export
StructureOperator <- function(pdb_entry_id, assembly_id = 1, search_mode = "STRICT_SHAPE_MATCH") {

  res <- list(
    value = list(entry_id = pdb_entry_id, assembly_id = as.character(assembly_id)),
    operator = StructureSearchMode[[search_mode]]
  )

  structure(res, class = c("StructureOperator", class(res)))

}

