#' Generate a JSON Query for RCSB PDB Data Retrieval
#'
#' This function constructs a JSON query for retrieving data from the RCSB Protein Data Bank (PDB).
#' It requires input parameters like IDs, data type, and properties to tailor the query for specific data retrieval needs.
#'
#' @param ids A vector of identifiers for which data needs to be retrieved.
#' @param data_type A string indicating the type of data to be queried, such as 'ENTRY', 'POLYMER_ENTITY', etc.
#' @param properties A list of properties to be included in the query. Each element of the list should be
#'   a character vector representing properties for the respective data type.
#' @return A string representing the generated JSON query formatted for PDB data retrieval.
#' @examples
#' ids <- c("1XYZ", "2XYZ")
#' properties <- list(cell = c("volume", "angle_beta"), exptl = c("method"))
#' json_query <- generate_json_query(ids, "ENTRY", properties)
#' json_query
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

  data_str <- sprintf("%s(%s: [\"%s\"])", DataType[[data_type]], q_str, paste(ids, collapse = "\", \""))

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
