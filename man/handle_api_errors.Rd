% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/handle_api_errors.R
\name{handle_api_errors}
\alias{handle_api_errors}
\title{Handle API Errors}
\usage{
handle_api_errors(response, url = "")
}
\arguments{
\item{response}{An HTTP response object.}

\item{url}{A string representing the requested URL (for more informative error messages).}
}
\value{
None. It stops execution if an error is detected.
}
\description{
This function checks for errors in the HTTP response and stops execution
if the request was not successful.
}
