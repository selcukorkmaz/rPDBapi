#' Send API Request
#'
#' This function sends an HTTP GET request to the specified URL and handles
#' any potential errors related to network issues or API failures.
#'
#' @param url A string representing the API endpoint URL.
#' @param verbosity A boolean flag to indicate whether to print status messages.
#' @return An HTTP response object.
#' @importFrom httr GET http_status
#' @export
send_api_request <- function(url, verbosity = TRUE) {
  if (verbosity) {
    message("Sending request to URL: ", url)
  }

  response <- tryCatch(
    {
      GET(url)
    },
    error = function(e) {
      stop("Network Error: Failed to send request. Error: ", e$message)
    }
  )

  return(response)
}
