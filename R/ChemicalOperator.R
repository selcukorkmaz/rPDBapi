# Descriptor Matching Criterion (similar to Enum in Python)
DescriptorMatchingCriterion <- c(
  "GRAPH_STRICT" = "graph-strict",
  "GRAPH_RELAXED" = "graph-relaxed",
  "GRAPH_RELAXED_STEREO" = "graph-relaxed-stereo",
  "FINGERPRINT_SIMILARITY" = "fingerprint-similarity"
)

# Chemical Operator Class
ChemicalOperator <- function(descriptor, matching_criterion = "GRAPH_STRICT") {
  descriptor_type <- ifelse(startsWith(descriptor, "InChI="), "InChI", "SMILES")

  list(
    descriptor = descriptor,
    matching_criterion = matching_criterion,
    descriptor_type = descriptor_type,
    to_dict = function() {
      list(
        value = descriptor,
        type = "descriptor",
        descriptor_type = descriptor_type,
        match_type = DescriptorMatchingCriterion[[matching_criterion]]
      )
    }
  )
}

# Example of creating a ChemicalOperator
# chem_operator <- ChemicalOperator(descriptor = "InChI=1S/C2H6O/c1-2-3/h3H,2H2,1H3", matching_criterion = "GRAPH_STRICT")
# print(chem_operator$to_dict())
