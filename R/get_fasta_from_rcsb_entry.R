#' Retrieve FASTA Sequence from PDB Entry or Specific Chain
#'
#' This function facilitates the retrieval of FASTA sequences from the RCSB Protein Data Bank (PDB) for a specified entry ID. It is versatile in that it can return either the full set of sequences associated with the entry or, if specified, the sequence corresponding to a particular chain within that entry. The function is designed to handle both cases seamlessly, making it a useful tool for bioinformaticians and structural biologists who need access to protein or nucleic acid sequences associated with PDB entries.
#'
#' @param rcsb_id A string representing the PDB ID for which the FASTA sequence is to be retrieved. This is the primary identifier of the entry in the PDB database.
#' @param chain_id A string representing the specific chain ID within the PDB entry for which the FASTA sequence is to be retrieved. If this parameter is set to NULL (the default), the function will return all sequences associated with the entry. The chain ID should match one of the chain identifiers in the PDB entry (e.g., "A", "B").
#' @param verbosity A boolean flag indicating whether to print status messages during the function execution. When set to TRUE (the default), the function will output messages detailing the progress and any issues encountered.
#' @return Depending on the input parameters:
#'     * If \code{chain_id} is NULL, the function returns a list of FASTA sequences associated with the provided \code{rcsb_id}, with organism names or chain descriptions as keys.
#'     * If \code{chain_id} is specified, the function returns a named list where the name corresponds to the \code{chain_id} and the value is the FASTA sequence for that specific chain.
#'     * If the specified \code{chain_id} is not found in the PDB entry, the function returns NULL and issues a warning.
#'
#' @details
#' This function queries the RCSB PDB database using the provided entry ID (\code{rcsb_id}) and optionally a chain ID (\code{chain_id}). The function sends an HTTP GET request to retrieve the corresponding FASTA file. It then parses the response and either returns all sequences associated with the entry or the sequence corresponding to the specified chain.
#'
#' The FASTA format is a common format for representing nucleotide or peptide sequences, in which sequences are represented by a single-letter code, and each sequence is preceded by a header line starting with a ">" character.
#'
#' The function handles common edge cases, such as missing chain IDs, and provides informative warnings to help users troubleshoot issues.
#'
#' @examples
#' # Example 1: Retrieve the full FASTA sequences for the entry 4HHB
#' all_sequences <- get_fasta_from_rcsb_entry("4HHB", verbosity = TRUE)
#' print(all_sequences)
#'
#' # Example 2: Retrieve the FASTA sequence for chain A of entry 4HHB
#' chain_a_sequence <- get_fasta_from_rcsb_entry("4HHB", chain_id = "A", verbosity = TRUE)
#' print(chain_a_sequence)
#'
#' @importFrom httr GET http_status content
#' @export

get_fasta_from_rcsb_entry <- function(rcsb_id, chain_id = NULL, verbosity = TRUE) {
  if (verbosity) {
    message(sprintf("Querying RCSB for the '%s' FASTA file.", rcsb_id))
  }

  response <- tryCatch(
    {
      GET(paste0(FASTA_BASE_URL, rcsb_id))
    },
    error = function(e) {
      stop("Failed to retrieve data from the RCSB PDB. Network error: ", e$message)
    }
  )

  if (http_status(response)$category != "Success") {
    stop("Request failed with status code ", http_status(response)$status, ": ", http_status(response)$message)
  }

  fasta_sequences <- tryCatch(
    {
      parse_fasta_text_to_list(content(response, "text", encoding = "UTF-8"))
    },
    error = function(e) {
      stop("Failed to parse FASTA response from RCSB PDB. The response may not be in the expected format. Error: ", e$message)
    }
  )

  if (is.null(chain_id)) {
    if (length(fasta_sequences) == 0) {
      stop("No FASTA sequences were found for the entry ID '", rcsb_id, "'.")
    }
    return(fasta_sequences)
  }

  # Find and return the sequence for the specified chain ID
  for (header in names(fasta_sequences)) {
    if (grepl(paste0("\\b", chain_id, "\\b"), header)) {
      chain_sequence <- fasta_sequences[[header]]
      result <- list()
      result[[chain_id]] <- chain_sequence
      return(result)
    }
  }

  stop(sprintf("Chain ID '%s' not found in PDB entry '%s'. Please check the chain ID and try again.", chain_id, rcsb_id))
}

