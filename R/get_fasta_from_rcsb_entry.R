FASTA_BASE_URL <- "https://www.rcsb.org/fasta/entry/"

# In R, PolymerEntity is represented simply as a character string.

FastaSequence <- function(entity_id, chains, sequence, fasta_header) {
  list(
    entity_id = entity_id,
    chains = chains,
    sequence = sequence,
    fasta_header = fasta_header
  )
}


parse_fasta_text_to_list <- function(raw_fasta_text) {
  fasta_sequence_chunks <- unlist(strsplit(raw_fasta_text, ">"))[-1]
  # fasta_list <- list()

  for (fasta_sequence_chunk in fasta_sequence_chunks) {
    chunk_lines <- unlist(strsplit(fasta_sequence_chunk, "\n"))
    fasta_header <- chunk_lines[1]
    fasta_sequence <- paste(chunk_lines[-1], collapse = "")
    header_segments <- unlist(strsplit(fasta_header, "\\|"))
    entity_id <- header_segments[1]
    chains <- unlist(strsplit(gsub("Chains? ", "", header_segments[2]), ","))

    # fasta_list[[length(fasta_list) + 1]] <- FastaSequence(entity_id, chains, fasta_sequence, fasta_header)
    fasta_list <- FastaSequence(entity_id, chains, fasta_sequence, fasta_header)
  }

  fasta_list
}


get_fasta_from_rcsb_entry <- function(rcsb_id, verbosity = TRUE) {
  if (verbosity) {
    message(sprintf("Querying RCSB for the '%s' FASTA file.", rcsb_id))
  }

  response <- httr::GET(paste0(FASTA_BASE_URL, rcsb_id))

  if (httr::http_status(response)$category != "Success") {
    warning("It appears the request failed with:", content(response, "text"))
    return(NULL)
  }

  parse_fasta_text_to_list(httr::content(response, "text"))
}

# Example Usage
# fasta_data <- get_fasta_from_rcsb_entry(c("2HHB"))
# print(fasta_data)
