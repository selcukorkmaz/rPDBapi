DescriptorMatchingCriterion <- c(
  GRAPH_STRICT = "graph-strict",
  GRAPH_RELAXED = "graph-relaxed",
  GRAPH_RELAXED_STEREO = "graph-relaxed-stereo",
  FINGERPRINT_SIMILARITY = "fingerprint-similarity"
)


#' Chemical Operator for SMILES/InChI Searches
#'
#' Creates an object representing a chemical search operator using SMILES or InChI descriptors.
#'
#' @param descriptor A valid SMILES or InChI string.
#' @param matching_criterion The criterion for matching, one of the values from DescriptorMatchingCriterion.
#'                           Defaults to "graph-strict".
#' @return A list representing a chemical operator, with a method `to_dict` for converting to a dictionary-like structure.
#' @examples
#' chemical_operator <- ChemicalOperator(descriptor = "C1=CC=CC=C1")
#' chemical_operator
ChemicalOperator <- function(descriptor, matching_criterion = "graph-strict") {
  if (startsWith(descriptor, "InChI=")) {
    descriptor_type <- "InChI"
  } else {
    descriptor_type <- "SMILES"
  }

  res <- list(
    value = descriptor,
    type = "descriptor",
    descriptor_type = descriptor_type,
    match_type = matching_criterion
  )

  structure(res, class = c("ChemicalOperator", class(res)))

}

# Example Usage
# chemical_operator <- ChemicalOperator(descriptor = "C1=CC=CC=C1", matching_criterion = DescriptorMatchingCriterion["GRAPH_STRICT"])
# dict_representation <- chemical_operator
# print(dict_representation)
