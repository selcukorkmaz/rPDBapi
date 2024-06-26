% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_fetcher.R
\name{data_fetcher}
\alias{data_fetcher}
\title{Fetch RCSB PDB Data Based on Specified Criteria}
\usage{
data_fetcher(
  id = NULL,
  data_type = "ENTRY",
  properties = NULL,
  return_as_dataframe = TRUE
)
}
\arguments{
\item{id}{An identifier or a list of identifiers for the data to be fetched.}

\item{data_type}{A string specifying the type of data to fetch.
Default is "ENTRY".}

\item{properties}{A list or dictionary of properties to be included in the data fetching process.}

\item{return_as_dataframe}{A boolean indicating whether to return the response as a dataframe.
Default is TRUE.}
}
\value{
Depending on the value of 'return_as_dataframe',
  this function returns either a dataframe or data in its original format.
}
\description{
This function fetches data based on a given identifier (ID), data type, and a set of properties.
It can return the data either in its original format or as a dataframe.
The function integrates several steps including validating IDs, generating a JSON query,
fetching the data, and formatting the response.
}
\examples{
properties <- list(cell = c("length_a", "length_b", "length_c"), exptl = c("method"))
data_fetcher(
id = c("4HHB"),
data_type = "ENTRY",
properties = properties,
return_as_dataframe = TRUE
)
}
