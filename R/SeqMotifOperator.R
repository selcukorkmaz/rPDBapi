#' Create a Sequence Motif Operator for RCSB PDB Searches
#'
#' The `SeqMotifOperator` function constructs an operator for searching sequence motifs within the RCSB Protein Data Bank (PDB).
#' This operator is used to specify a search pattern, the type of biological sequence, and the pattern-matching method to be applied in the search.
#'
#' @param pattern A string representing the motif pattern to search for. This can be a simple string or a more complex pattern, depending on the `pattern_type`.
#' @param sequence_type A string indicating the type of sequence being searched. Accepted values are 'DNA', 'RNA', or 'PROTEIN'.
#' @param pattern_type A string indicating the pattern matching method to use. Options include 'SIMPLE' for basic patterns, 'PROSITE' for PROSITE-style patterns, and 'REGEX' for regular expressions.
#' @return An object of class `SeqMotifOperator` that encapsulates the specified search criteria. This object can be used as part of a search query within the RCSB PDB system.
#' @examples
#' # Example of creating a sequence motif operator to search for a DNA motif using a regular expression
#' seq_motif_operator <- SeqMotifOperator(
#'   pattern = "A[TU]G",
#'   sequence_type = "DNA",
#'   pattern_type = "REGEX"
#' )
#' print(seq_motif_operator)
#' @export

SeqMotifOperator <- function(pattern, sequence_type, pattern_type) {

  res <- list(
    value = pattern,
    pattern_type = PatternType[[pattern_type]],
    target = SequenceType[[sequence_type]]
  )

  structure(res, class = c("SeqMotifOperator", class(res)))

}

