#' Parse API Response
#'
#' This function parses the content of an HTTP response based on the specified format.
#' It supports JSON and plain text formats.
#'
#' @param response An HTTP response object.
#' @param format A string indicating the expected response format ("json" or "text").
#' @return Parsed content from the response.
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
#' @export
parse_response <- function(response, format = "json") {
  parsed_content <- tryCatch(
    {
      if (format == "json") {
        fromJSON(content(response, "text", encoding = "UTF-8"))
      } else if (format == "text") {
        content(response, "text", encoding = "UTF-8")
      } else {
        stop("Unsupported format for parsing the response: ", format)
      }
    },
    error = function(e) {
      stop("Parsing Error: Failed to parse the response. Error: ", e$message)
    }
  )

  return(parsed_content)
}
