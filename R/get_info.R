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
#' # Example usage:
#' # info <- get_info(pdb_id = c"4hhb")
#' @export
get_info <- function(pdb_id, url_root = 'https://data.rcsb.org/rest/v1/core/entry/') {
  pdb_id <- gsub(":", "/", pdb_id)  # Replace old entry identifier
  url <- paste0(url_root, pdb_id)
  response <- GET(url)

  if (http_status(response)$category != "Success") {
    stop(content(response)$message)
  }

  out <- fromJSON(content(response, "text"))
  return(out)
}

# Alias the function
get_all_info <- get_info
describe_pdb <- get_info
get_entity_info <- get_info
