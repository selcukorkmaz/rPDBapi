#' Create a FASTA Sequence Data Structure
#'
#' This function constructs a FASTA sequence data structure from given parameters.
#' It is typically used to format and store FASTA sequence information.
#'
#' @param entity_id A string representing the entity ID of the sequence.
#' @param chains A character vector of chain identifiers associated with the sequence.
#' @param sequence A string representing the nucleotide or amino acid sequence.
#' @param fasta_header A string representing the header of the FASTA sequence.
#' @return A list representing the FASTA sequence, including entity ID, chains, the sequence itself, and the FASTA header.
#' @examples
#' fasta_data <- FastaSequence("1XYZ", c("A", "B"), "MVLSPADKT", "header_info")
#' @export
FastaSequence <- function(entity_id, chains, sequence, fasta_header) {
  list(
    entity_id = entity_id,
    chains = chains,
    sequence = sequence,
    fasta_header = fasta_header
  )
}

#' Parse Raw FASTA Text into a List of Sequences
#'
#' This function parses raw FASTA text into a structured list format.
#' It splits the FASTA text into individual sequences and extracts relevant information
#' like entity ID, chains, sequence, and header.
#'
#' @param raw_fasta_text A string containing the raw FASTA text to be parsed.
#' @return A list of FASTA sequences, each as a separate list element with entity ID, chains, sequence, and header.
#' @examples
#' raw_fasta_text <- ">1XYZ|Chains A, B\nMVLSPADKT...\n>2XYZ|Chain C\nGVLSADFT..."
#' fasta_list <- parse_fasta_text_to_list(raw_fasta_text)
#' @export
parse_fasta_text_to_list <- function(raw_fasta_text) {
  # Splitting the raw FASTA text into sequences
  fasta_sequence_chunks <- strsplit(raw_fasta_text, ">")[[1]][-1]

  fasta_list <- list()
  for (fasta_sequence_chunk in fasta_sequence_chunks) {
    chunk_lines <- strsplit(fasta_sequence_chunk, "\n")[[1]]
    fasta_header <- chunk_lines[1]
    fasta_sequence <- paste(chunk_lines[-1], collapse = "")

    header_segments <- strsplit(fasta_header, "\\|")[[1]]
    entity_id <- header_segments[1]
    chains <- strsplit(gsub("Chains? ", "", header_segments[2]), ",")[[1]]

    # Append FastaSequence to the list
    fasta_list <- list(
      entity_id = entity_id,
      chains = chains,
      sequence = fasta_sequence,
      fasta_header = fasta_header
    )
  }
  fasta_list
}


