#' Handle API Errors
#'
#' This function checks for errors in the HTTP response and stops execution
#' if the request was not successful.
#'
#' @param response An HTTP response object.
#' @param url A string representing the requested URL (for more informative error messages).
#' @return None. It stops execution if an error is detected.
#' @importFrom httr http_status
#' @export
handle_api_errors <- function(response, url = "") {
  if (http_status(response)$category != "Success") {
    stop("HTTP Error: Request to ", url, " failed with status: ",
         http_status(response)$status, " - ", http_status(response)$message)
  }
}
