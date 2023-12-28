# Sequence Types (similar to Enum in Python)
SequenceType <- c("DNA" = "pdb_dna_sequence", "RNA" = "pdb_rna_sequence", "PROTEIN" = "pdb_protein_sequence")

# Cannot Autoresolve Sequence Type Exception
# In R, custom exceptions are not typical; use standard error messaging
cannot_autoresolve_sequence_type_error <- function(message) {
  stop(message)
}

# Sequence Operator Class
SequenceOperator <- function(sequence, sequence_type = NULL, evalue_cutoff = 100, identity_cutoff = 0.95) {
  if (is.null(sequence_type)) {
    sequence_type <- autoresolve_sequence_type(sequence)
  }

  list(
    sequence = sequence,
    sequence_type = sequence_type,
    evalue_cutoff = evalue_cutoff,
    identity_cutoff = identity_cutoff,
    to_dict = function() {
      list(
        evalue_cutoff = evalue_cutoff,
        identity_cutoff = identity_cutoff,
        target = SequenceType[[sequence_type]],
        value = sequence
      )
    }
  )
}

# Autoresolve Sequence Type Function
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
    cannot_autoresolve_sequence_type_error(
      sprintf("Sequence is ambiguous as to its SequenceType: `%s`", sequence)
    )
  }
}


# Example of creating a SequenceOperator
# seq_operator <- SequenceOperator(sequence = "ATCG", evalue_cutoff = 100, identity_cutoff = 0.95)
# print(seq_operator$to_dict())
