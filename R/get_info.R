#' Retrieve Information for a Given PDB ID
#'
#' This function looks up all information about a given PDB ID using the REST API.
#' It handles JSON data and HTTP requests and converts old entry identifiers.
#'
#' @param pdb_id A 4-character string specifying a PDB entry of interest.
#' @param url_root The root URL for the specific request type. Default is 'https://data.rcsb.org/rest/v1/core/entry/'.
#' @return An ordered dictionary (list in R) object corresponding to entry information.
#'         Returns NULL if retrieval fails.
#' @importFrom httr GET http_status
#' @importFrom jsonlite fromJSON
#' @examples
#' get_info(pdb_id = "4HHB")
#' @export
get_info <- function(pdb_id, url_root = 'https://data.rcsb.org/rest/v1/core/entry/') {
  pdb_id <- gsub(":", "/", pdb_id)  # Replace old entry identifier
  url <- paste0(url_root, pdb_id)

  response <- tryCatch(
    {
      GET(url)
    },
    error = function(e) {
      warning("Failed to retrieve data: ", e$message)
      return(NULL)
    }
  )

  if (is.null(response)) {
    return(NULL)
  }

  if (http_status(response)$category != "Success") {
    warning("Request failed with: ", http_status(response)$reason)
    return(NULL)
  }

  out <- tryCatch(
    {
      fromJSON(content(response, "text", encoding = "UTF-8"))
    },
    error = function(e) {
      warning("Failed to parse JSON response: ", e$message)
      return(NULL)
    }
  )

  return(out)
}

# Alias the function
get_all_info <- get_info
describe_pdb <- get_info
get_entity_info <- get_info
