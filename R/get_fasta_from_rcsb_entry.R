#' Retrieve FASTA Sequences from PDB Entry
#'
#' This function fetches FASTA sequences from the RCSB Protein Data Bank (PDB) for a specified entry ID.
#' It sends an HTTP request to the PDB and processes the response to extract FASTA sequences.
#'
#' @param rcsb_id A string representing the PDB ID for which the FASTA sequence is to be retrieved.
#' @param verbosity A boolean flag indicating whether to print status messages during the function execution.
#'   Defaults to TRUE.
#' @return A list of FASTA sequences associated with the provided RCSB entry ID.
#' @export
#' @importFrom httr GET http_status
#' @importFrom jsonlite fromJSON
get_fasta_from_rcsb_entry <- function(rcsb_id, verbosity = TRUE) {
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
