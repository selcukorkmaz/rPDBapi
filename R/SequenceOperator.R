#' Create a Sequence Operator for Sequence-Based Searches
#'
#' The `SequenceOperator` function constructs an operator for performing sequence-based searches within the RCSB Protein Data Bank (PDB).
#' This operator allows users to specify a nucleotide or protein sequence, define the type of sequence, and set thresholds for e-value and identity in the search process.
#'
#' @param sequence A string representing the nucleotide or protein sequence to search for. The sequence should be provided in standard IUPAC format.
#' @param sequence_type Optional: A string indicating the type of sequence. Accepted values are 'DNA', 'RNA', or 'PROTEIN'.
#'   If not provided, the sequence type is automatically determined based on the characters present in the sequence using the `autoresolve_sequence_type` function.
#' @param evalue_cutoff A numeric value for the e-value cutoff in the search. This defines the threshold for statistical significance of the search results. Default is 100.
#' @param identity_cutoff A numeric value for the identity cutoff in the search. This sets the minimum percentage of identity required for a match to be considered. Default is 0.95.
#' @return An object of class `SequenceOperator` that encapsulates the search criteria for sequence-based queries within the RCSB PDB.
#' @examples
#' # Example of creating a sequence operator for a protein sequence with specific cutoffs
#' seq_operator <- SequenceOperator(
#'   sequence = "MVLSPADKTNVKAAW",
#'   sequence_type = "PROTEIN",
#'   evalue_cutoff = 10,
#'   identity_cutoff = 0.90
#' )
#' print(seq_operator)
#'
#' # Example of creating a sequence operator with automatic sequence type detection
#' seq_operator_auto <- SequenceOperator(
#'   sequence = "ATGCGTACGTAGC",
#'   evalue_cutoff = 50,
#'   identity_cutoff = 0.85
#' )
#' print(seq_operator_auto)
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
#' The `autoresolve_sequence_type` function analyzes the characters in a given sequence to determine whether it is a DNA, RNA, or protein sequence.
#' The function uses standard IUPAC nucleotide and amino acid codes to classify the sequence based on its composition.
#'
#' @param sequence A string representing the nucleotide or protein sequence to be analyzed. The sequence should be composed of characters corresponding to standard IUPAC codes.
#' @return A string indicating the resolved sequence type: 'DNA', 'RNA', or 'PROTEIN'.
#'   If the sequence contains ambiguous characters or does not fit clearly into one of these categories, an error is thrown.
#' @examples
#' # Example of determining the sequence type for a DNA sequence
#' seq_type_dna <- autoresolve_sequence_type("ATGCGTACGTAGC")
#' print(seq_type_dna)  # Should return "DNA"
#'
#' # Example of determining the sequence type for a protein sequence
#' seq_type_protein <- autoresolve_sequence_type("MVLSPADKTNVKAAW")
#' print(seq_type_protein)  # Should return "PROTEIN"
#'
#' # Example of an ambiguous sequence that causes an error
#' # autoresolve_sequence_type("ATGB")  # Should throw an error due to ambiguity
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
