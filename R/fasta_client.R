FASTA_BASE_URL <- "https://www.rcsb.org/fasta/entry/"

# PolymerEntity Type Alias
# In R, we simply use character strings to represent PolymerEntity.

FastaSequence <- function(entity_id, chains, sequence, fasta_header) {
  list(
    entity_id = entity_id,
    chains = chains,
    sequence = sequence,
    fasta_header = fasta_header
  )
}


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


get_fasta_from_rcsb_entry <- function(rcsb_id, verbosity = TRUE) {
  # Assuming FASTA_BASE_URL and parse_fasta_text_to_list are defined
  if (verbosity) {
    message(sprintf("Querying RCSB for the '%s' FASTA file.", rcsb_id))
  }

  response <- GET(paste0(FASTA_BASE_URL, rcsb_id))

  if (http_status(response)$category != "Success") {
    stop("Request failed with:", content(response, "text", encoding = "UTF-8"))
  }

  fasta_sequences <- parse_fasta_text_to_list(content(response, "text", encoding = "UTF-8"))
  return(fasta_sequences)
}

# Example Usage
# fasta_sequences <- get_fasta_from_rcsb_entry("5RU3")
# print(fasta_sequences)
