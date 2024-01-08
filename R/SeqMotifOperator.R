# Sequence Types (similar to Enum in Python)
SequenceType <- c("DNA" = "pdb_dna_sequence", "RNA" = "pdb_rna_sequence", "PROTEIN" = "pdb_protein_sequence")

# Pattern Types
PatternType <- c("SIMPLE" = "simple", "PROSITE" = "prosite", "REGEX" = "regex")

# SeqMotif Operator Class
SeqMotifOperator <- function(pattern, sequence_type, pattern_type) {

  res <- list(
    value = pattern,
    pattern_type = PatternType[[pattern_type]],
    target = SequenceType[[sequence_type]]
  )

  structure(res, class = c("SeqMotifOperator", class(res)))

}

# Example of creating a SeqMotifOperator
# seq_motif_operator <- SeqMotifOperator(pattern = "A[TU]G", sequence_type = "DNA", pattern_type = "REGEX")
# print(seq_motif_operator$to_dict())
