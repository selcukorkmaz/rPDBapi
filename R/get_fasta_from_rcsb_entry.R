#' Retrieve FASTA Sequences from PDB Entry
#'
#' This function fetches FASTA sequences from the RCSB Protein Data Bank (PDB) for a specified entry ID.
#' It sends an HTTP request to the PDB and processes the response to extract FASTA sequences.
#'
#' @param rcsb_id A string representing the PDB ID for which the FASTA sequence is to be retrieved.
#' @param verbosity A boolean flag indicating whether to print status messages during the function execution.
#'   Defaults to TRUE.
#' @return A list of FASTA sequences associated with the provided RCSB entry ID.
#' @importFrom httr GET http_status
#' @importFrom jsonlite fromJSON
#' @examples
#' get_fasta_from_rcsb_entry(c("4HHB"), verbosity = TRUE)
#' @export
get_fasta_from_rcsb_entry <- function(rcsb_id, verbosity = TRUE) {
  if (verbosity) {
    message(sprintf("Querying RCSB for the '%s' FASTA file.", rcsb_id))
  }

  response <- tryCatch(
    {
      GET(paste0(FASTA_BASE_URL, rcsb_id))
    },
    error = function(e) {
      warning("Failed to retrieve data: ", e$message)
      return(NULL)
    }
  )

  if (is.null(response)) {
    return(NULL)
  }

  if (http_status(response)$category != "Success") {
    warning("Request failed with: ", content(response, "text", encoding = "UTF-8"))
    return(NULL)
  }

  fasta_sequences <- tryCatch(
    {
      parse_fasta_text_to_list(content(response, "text", encoding = "UTF-8"))
    },
    error = function(e) {
      warning("Failed to parse FASTA response: ", e$message)
      return(NULL)
    }
  )

  return(fasta_sequences)
}
