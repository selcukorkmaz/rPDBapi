#' Helper Function: Parse FASTA Text to List Grouped by Header
#'
#' This function parses a FASTA-formatted text into a list of sequences,
#' where each sequence is keyed by the header (which includes organism name and chain information).
#'
#' @param fasta_text A string containing FASTA-formatted text.
#' @return A list where each element is a FASTA sequence keyed by the header.
parse_fasta_text_to_list <- function(fasta_text) {
  fasta_lines <- strsplit(fasta_text, "\n")[[1]]
  sequences <- list()
  current_header <- NULL
  current_sequence <- ""

  for (line in fasta_lines) {
    if (startsWith(line, ">")) {
      if (!is.null(current_header)) {
        sequences[[current_header]] <- current_sequence
      }
      # Set the new header line
      current_header <- sub("^>", "", line)
      current_sequence <- ""
    } else {
      current_sequence <- paste0(current_sequence, line)
    }
  }

  # Add the last sequence to the list
  if (!is.null(current_header)) {
    sequences[[current_header]] <- current_sequence
  }

  return(sequences)
}
