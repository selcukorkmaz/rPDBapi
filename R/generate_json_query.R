#' Generate a JSON Query for RCSB PDB Data Retrieval
#'
#' This function constructs a JSON query string tailored for retrieving data from the RCSB Protein Data Bank (PDB).
#' It is designed to accommodate a variety of data types, such as entries, polymer entities, assemblies, and chemical components.
#' The function is particularly useful when specific properties need to be queried across multiple PDB identifiers.
#'
#' @param ids A character vector of identifiers corresponding to the data you wish to retrieve. These identifiers should
#'   match the type of data specified in the `data_type` argument. For example, if `data_type` is 'ENTRY', the identifiers
#'   should be PDB entry IDs like "1XYZ" or "2XYZ".
#' @param data_type A string indicating the type of data to query. The function supports the following types:
#'   \describe{
#'     \item{"ENTRY"}{Refers to the entire PDB entry, typically associated with the full structural dataset.}
#'     \item{"POLYMER_ENTITY"}{Represents specific polymer entities, such as proteins or nucleic acids, within an entry.}
#'     \item{"BRANCHED_ENTITY"}{Refers to branched entities, often used to describe glycans or other complex molecules.}
#'     \item{"NONPOLYMER_ENTITY"}{Describes non-polymeric entities, such as ligands or cofactors.}
#'     \item{"POLYMER_ENTITY_INSTANCE"}{Specific instances of polymer entities, usually chains within a structure.}
#'     \item{"BRANCHED_ENTITY_INSTANCE"}{Instances of branched entities within a structure.}
#'     \item{"NONPOLYMER_ENTITY_INSTANCE"}{Instances of non-polymeric entities within a structure.}
#'     \item{"ASSEMBLY"}{Refers to macromolecular assemblies, which can include multiple chains and entities.}
#'     \item{"CHEMICAL_COMPONENT"}{Specific chemical components, often small molecules or ligands, within the PDB.}
#'   }
#' @param properties A named list where each element represents a property to be included in the query for the corresponding
#'   `data_type`. Each element of the list should be a character vector containing the specific properties you wish to retrieve.
#'   For example, for `data_type = "ENTRY"`, properties might include `cell = c("volume", "angle_beta")` to retrieve cell volume
#'   and angle beta of the unit cell.
#' @return A string representing the generated JSON query. This string is formatted according to the requirements of the RCSB PDB API
#'   and can be used directly in a GraphQL query to retrieve the specified data.
#'
#' @details
#' The function is designed to be flexible and extensible, allowing users to construct complex queries with multiple properties
#' and data types. The function internally maps the `data_type` to the appropriate query format and ensures that the JSON string
#' is properly constructed. The `properties` argument should align with the data type; otherwise, the query might not return
#' the desired results.
#'
#' The `generate_json_query` function is particularly useful for researchers and bioinformaticians who need to retrieve specific
#' datasets from the PDB, especially when dealing with large-scale structural biology data. The generated JSON query can be used
#' in subsequent API calls to fetch the required data in a structured and efficient manner.
#'
#' @examples
#' # Example 1: Generate a query for PDB entries with specific properties
#' ids <- c("1XYZ", "2XYZ")
#' properties <- list(cell = c("volume", "angle_beta"), exptl = c("method"))
#' json_query <- generate_json_query(ids, "ENTRY", properties)
#' print(json_query)
#'
#' # Example 2: Generate a query for chemical components with specified properties
#' ids <- c("ATP", "NAD")
#' properties <- list(chem_comp = c("formula_weight", "type"))
#' json_query <- generate_json_query(ids, "CHEMICAL_COMPONENT", properties)
#' print(json_query)
#'
#' # Example 3: Generate a query for polymer entities within a PDB entry
#' ids <- c("1XYZ_1", "2XYZ_2")
#' properties <- list(entity_src_gen = c("organism_scientific", "gene_src_common"))
#' json_query <- generate_json_query(ids, "POLYMER_ENTITY", properties)
#' print(json_query)
#'
#' @export


generate_json_query <- function(ids, data_type, properties) {
  if (length(properties) == 0) {
    stop("ERROR: no properties given to generate JSON query.")
  }

  q_str <- switch(data_type,
                  ENTRY = "entry_ids",
                  POLYMER_ENTITY = "entity_ids",
                  BRANCHED_ENTITY = "entity_ids",
                  NONPOLYMER_ENTITY = "entity_ids",
                  POLYMER_ENTITY_INSTANCE = "instance_ids",
                  BRANCHED_ENTITY_INSTANCE = "instance_ids",
                  NONPOLYMER_ENTITY_INSTANCE = "instance_ids",
                  ASSEMBLY = "assembly_ids",
                  CHEMICAL_COMPONENT = "comp_ids")

  data_str <- paste0(DataType[[data_type]], "(", q_str, ": [\"", paste(ids, collapse = "\", \""), "\"])")

  props_str <- sapply(properties, function(x) {
    if (length(x) == 0) {
      return("")
    } else {
      return(paste0("{", paste(x, collapse = ", "), "}"))
    }
  }, USE.NAMES = FALSE)

  props_str <- paste(names(properties), props_str, collapse = ", ")

  json_query <- paste0(list(query = paste0("{", data_str, "{", props_str, "}}")))
  return(json_query)
}
