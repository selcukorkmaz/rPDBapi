#' Retrieve Information for a Given PDB ID
#'
#' This function retrieves comprehensive information for a specified PDB (Protein Data Bank) entry by querying the RCSB PDB RESTful API.
#' The function handles HTTP requests, processes JSON responses, and can manage legacy PDB identifiers.
#' It is particularly useful for obtaining all available data related to a specific PDB entry, which can include metadata,
#' structural details, experimental methods, and more.
#'
#' @param pdb_id A string specifying the PDB entry of interest. The `pdb_id` should be a 4-character alphanumeric code,
#'   representing the unique identifier of a PDB entry (e.g., "1XYZ").
#'   If a legacy PDB identifier is provided in the format `PDB_ID:CHAIN_ID`, it will be automatically converted to the
#'   new format for querying.
#' @param url_root The root URL for the API request. This parameter allows flexibility in querying different endpoints
#'   within the RCSB PDB API. The default is 'https://data.rcsb.org/rest/v1/core/entry/', which targets the core entry
#'   information endpoint. Advanced users can modify this to query other endpoints if needed.
#' @return A list object (an ordered dictionary in R) containing detailed information about the specified PDB entry.
#'   The returned list includes various data fields, depending on the content available for the entry. For example,
#'   it may contain information about the structure's authors, resolution, experiment type, macromolecules, ligands, etc.
#'   If the data retrieval fails at any stage (e.g., network issues, invalid PDB ID, API downtime), the function will return `NULL`
#'   and provide an informative error message.
#' @importFrom httr GET http_status
#' @importFrom jsonlite fromJSON
#' @examples
#' pdb_info <- get_info(pdb_id = "1XYZ")
#' print(pdb_info)
#'
#' @details
#' The `get_info` function is versatile and designed for researchers who need to extract detailed structural and
#' experimental information from the RCSB PDB. The function is robust, providing error handling for various scenarios,
#' including network failures, incorrect PDB IDs, and API errors. It automatically manages legacy PDB IDs, ensuring
#' compatibility with the latest API standards.
#'
#' The output is a structured list that can be easily parsed or manipulated for further analysis, making it an essential
#' tool for bioinformaticians and structural biologists working with PDB data.
#'
#' The function also offers flexibility in querying different parts of the RCSB PDB API by adjusting the `url_root`
#' parameter, allowing users to target specific datasets within the PDB.
#'
#' @export
get_info <- function(pdb_id, url_root = 'https://data.rcsb.org/rest/v1/core/entry/') {
  # Replace old entry identifier format with the new one
  pdb_id <- gsub(":", "/", pdb_id)
  url <- paste0(url_root, pdb_id)

  # Attempt to retrieve data from the specified URL
  response <- tryCatch(
    {
      GET(url)
    },
    error = function(e) {
      stop("Failed to retrieve data from the RCSB PDB. Error: ", e$message)
    }
  )

  # Check if the response is successful
  if (http_status(response)$category != "Success") {
    stop("Request for PDB ID '",  pbd_id,  "' failed with message: ", http_status(response)$message)
  }

  # Attempt to parse the JSON response
  out <- tryCatch(
    {
      fromJSON(content(response, "text", encoding = "UTF-8"))
    },
    error = function(e) {
      stop("Failed to parse JSON response from the RCSB PDB for PDB ID '", pdb_id, "'. Error: ", e$message)
    }
  )

  # Return the parsed output
  return(out)
}

# Alias the function
utils::globalVariables("pbd_id")
get_all_info <- get_info
describe_pdb <- get_info
get_entity_info <- get_info
