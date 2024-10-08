% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_fasta_from_rcsb_entry.R
\name{get_fasta_from_rcsb_entry}
\alias{get_fasta_from_rcsb_entry}
\title{Retrieve FASTA Sequence from PDB Entry or Specific Chain}
\usage{
get_fasta_from_rcsb_entry(
  rcsb_id,
  chain_id = NULL,
  verbosity = TRUE,
  fasta_base_url = FASTA_BASE_URL
)
}
\arguments{
\item{rcsb_id}{A string representing the PDB ID for which the FASTA sequence is to be retrieved. This is the primary identifier of the entry in the PDB database.}

\item{chain_id}{A string representing the specific chain ID within the PDB entry for which the FASTA sequence is to be retrieved. If \code{chain_id} is NULL (the default), the function will return all sequences associated with the entry. The chain ID should match one of the chain identifiers in the PDB entry (e.g., "A", "B").}

\item{verbosity}{A boolean flag indicating whether to print status messages during the function execution. When set to \code{TRUE} (the default), the function will output messages detailing the progress and any issues encountered.}

\item{fasta_base_url}{A string representing the base URL for the FASTA retrieval. By default, this is set to the global constant \code{FASTA_BASE_URL}, but users can specify a different URL if needed.}
}
\value{
* If \code{chain_id} is NULL, the function returns a list of FASTA sequences associated with the provided \code{rcsb_id}, where organism names or chain descriptions are used as keys.
* If \code{chain_id} is specified, the function returns a character string representing the FASTA sequence for that specific chain.
* If the specified \code{chain_id} is not found in the PDB entry, the function will stop execution with an informative error message.
}
\description{
This function retrieves FASTA sequences from the RCSB Protein Data Bank (PDB) for a specified entry ID (\code{rcsb_id}). It can return either the full set of sequences associated with the entry or, if specified, the sequence corresponding to a particular chain within that entry. This flexibility makes it a useful tool for bioinformaticians and structural biologists needing access to protein or nucleic acid sequences.
}
\details{
The function queries the RCSB PDB database using the provided entry ID (\code{rcsb_id}) and optionally a chain ID (\code{chain_id}). It sends an HTTP GET request to retrieve the corresponding FASTA file. The response is then parsed into a list of sequences. If a chain ID is provided, the function will return only the sequence corresponding to that chain. If no chain ID is provided, all sequences are returned.

If a request fails, the function provides informative error messages. In the case of a network failure, the function will stop execution with a clear error message. Additionally, if the chain ID does not exist within the entry, the function will return an appropriate error message indicating that the chain was not found.

The function also supports passing a custom base URL for the FASTA file retrieval, providing flexibility for users working with different PDB mirrors or services.
}
\examples{
# Example 1: Retrieve all FASTA sequences for the entry 4HHB
all_sequences <- get_fasta_from_rcsb_entry("4HHB", verbosity = TRUE)
print(all_sequences)

# Example 2: Retrieve the FASTA sequence for chain A of entry 4HHB
chain_a_sequence <- get_fasta_from_rcsb_entry("4HHB", chain_id = "A", verbosity = TRUE)
print(chain_a_sequence)

}
