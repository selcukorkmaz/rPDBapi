#' Create a Sequence Operator for Sequence-Based Searches
#'
#' Constructs a SequenceOperator object for use in sequence-based searches within the RCSB PDB.
#' This operator allows specifying a sequence, its type, and cutoffs for e-value and identity.
#'
#' @param sequence A string representing the sequence to search for.
#' @param sequence_type Optional: a string indicating the type of sequence ('DNA', 'RNA', or 'PROTEIN').
#'   If not provided, the type is autoresolved.
#' @param evalue_cutoff A numeric value for the e-value cutoff in the search, default is 100.
#' @param identity_cutoff A numeric value for the identity cutoff in the search, default is 0.95.
#' @return An object of class 'SequenceOperator' representing the sequence operator.
#' @export
SequenceOperator <- function(sequence, sequence_type = NULL, evalue_cutoff = 100, identity_cutoff = 0.95) {
  if (is.null(sequence_type)) {
    sequence_type <- autoresolve_sequence_type(sequence)
  }

  res <- list(
    evalue_cutoff = evalue_cutoff,
    identity_cutoff = identity_cutoff,
    target = SequenceType[[sequence_type]],
    value = sequence
  )

  # Use structure to set the class attribute
  structure(res, class = c("SequenceOperator", class(res)))
}


#' Automatically Determine the Sequence Type
#'
#' This function determines the type of a given sequence (DNA, RNA, or PROTEIN) based on its characters.
#'
#' @param sequence A string representing the sequence to be analyzed.
#' @return A string indicating the resolved sequence type.
#' @export
autoresolve_sequence_type <- function(sequence) {
  unique_letters <- unique(strsplit(sequence, "")[[1]])

  dna_letter_set <- c("A", "T", "C", "G")
  rna_letter_set <- c("A", "U", "C", "G")
  protein_letter_set <- strsplit("ABCDEFGHIKLMNPQRSTVWXYZ", "")[[1]]
  protein_fingerprint_set <- strsplit("BDEFHIKLMNPQRSVWXYZ", "")[[1]]

  if (all(unique_letters %in% dna_letter_set) && "T" %in% unique_letters) {
    return("DNA")
  } else if (all(unique_letters %in% rna_letter_set) && "U" %in% unique_letters) {
    return("RNA")
  } else if (all(unique_letters %in% protein_letter_set) && any(unique_letters %in% protein_fingerprint_set)) {
    return("PROTEIN")
  } else {
      stop("Sequence is ambiguous as to its SequenceType: ", sequence)
    
  }
}
