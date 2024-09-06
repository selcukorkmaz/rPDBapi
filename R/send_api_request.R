#' Send API Request to a Specified URL
#'
#' This function sends an HTTP request (GET or POST) to the specified URL. It supports optional request bodies for POST requests, customizable encoding, and content type for API interactions. The function is designed to be a general-purpose API handler for use in querying external APIs.
#'
#' @param url A string representing the target URL for the API request.
#' @param method A string specifying the HTTP method to use. The default is "GET", but "POST" can also be used.
#' @param body Optional: The body of the request, typically required for POST requests. Default is \code{NULL}.
#' @param encode A string representing the encoding type of the body for POST requests. Default is \code{"json"}.
#' @param content_type A string specifying the content type for POST requests. Default is \code{"application/json"}.
#' @param verbosity Logical flag indicating whether to print status messages during the function execution. Default is \code{TRUE}.
#'
#' @return A response object from the \code{httr} package representing the server's response to the API request.
#'
#' @details
#' The \code{send_api_request} function is a flexible tool for handling API interactions. It supports both GET and POST methods and provides optional parameters for encoding and content type, making it suitable for a wide range of API requests.
#'
#' If a network error occurs during the request, the function will throw an error with a detailed message about the failure.
#'
#' @importFrom httr GET POST content_type
#' @export

send_api_request <- function(url, method = "GET", body = NULL, encode = "json", content_type = "application/json", verbosity = TRUE) {

  if (verbosity) {
    message("Sending ", method, " request to URL: ", url)
  }

  response <- tryCatch(
    {
      if (toupper(method) == "POST") {
        POST(url, body = body, encode = encode, content_type(content_type))
      } else if (toupper(method) == "GET") {
        GET(url)
      } else {
        stop("Unsupported HTTP method: ", method)
      }
    },
    error = function(e) {
      stop("Network Error: Failed to send request. Error: ", e$message)
    }
  )

  return(response)
}
