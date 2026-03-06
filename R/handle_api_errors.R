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
  status <- http_status(response)
  if (status$category != "Success") {
    rpdbapi_abort(
      paste0("HTTP Error: Request to ", url, " failed with status: ", status$status, " - ", status$message),
      class = "rPDBapi_error_http",
      function_name = "handle_api_errors",
      url = url,
      status = status$status,
      status_message = status$message
    )
  }
}
