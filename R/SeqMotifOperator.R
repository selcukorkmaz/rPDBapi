
#' Create a Sequence Motif Operator for Searches
#'
#' Constructs a SeqMotifOperator object for use in sequence motif searches within the RCSB PDB.
#' This operator allows specifying a pattern, sequence type, and pattern type.
#'
#' @param pattern A string representing the motif pattern to search for.
#' @param sequence_type A string indicating the type of sequence ('DNA', 'RNA', or 'PROTEIN').
#' @param pattern_type A string indicating the type of pattern ('SIMPLE', 'PROSITE', or 'REGEX').
#' @return An object of class 'SeqMotifOperator' representing the sequence motif operator.
#' @examples
#' seq_motif_operator <- SeqMotifOperator(pattern = "A[TU]G",
#'                                        sequence_type = "DNA",
#'                                        pattern_type = "REGEX")
#' seq_motif_operator
#'
#'@export

SeqMotifOperator <- function(pattern, sequence_type, pattern_type) {

  res <- list(
    value = pattern,
    pattern_type = PatternType[[pattern_type]],
    target = SequenceType[[sequence_type]]
  )

  structure(res, class = c("SeqMotifOperator", class(res)))

}

