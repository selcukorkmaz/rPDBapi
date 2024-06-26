% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_fasta_from_rcsb_entry.R
\name{get_fasta_from_rcsb_entry}
\alias{get_fasta_from_rcsb_entry}
\title{Retrieve FASTA Sequences from PDB Entry}
\usage{
get_fasta_from_rcsb_entry(rcsb_id, verbosity = TRUE)
}
\arguments{
\item{rcsb_id}{A string representing the PDB ID for which the FASTA sequence is to be retrieved.}

\item{verbosity}{A boolean flag indicating whether to print status messages during the function execution.
Defaults to TRUE.}
}
\value{
A list of FASTA sequences associated with the provided RCSB entry ID.
}
\description{
This function fetches FASTA sequences from the RCSB Protein Data Bank (PDB) for a specified entry ID.
It sends an HTTP request to the PDB and processes the response to extract FASTA sequences.
}
\examples{
get_fasta_from_rcsb_entry(c("4HHB"), verbosity = TRUE)
}
