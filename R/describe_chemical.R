#' Describe Chemical Compound from RCSB PDB
#'
#' Retrieves detailed information about a chemical compound from the RCSB Protein Data Bank (PDB) based on its chemical ID.
#'
#' @param chem_id A string representing the 3-character chemical ID. This ID is typically an alphanumeric string used to uniquely identify ligands, cofactors, or other small molecules within macromolecular structures. The string must not exceed 3 characters.
#' @return A list containing detailed information about the chemical compound. This list includes various fields such as:
#' \describe{
#'   \item{rcsb_chem_comp_descriptor}{A sublist containing chemical descriptors like SMILES, InChI strings, molecular weight, and other chemical properties.}
#'   \item{rcsb_chem_comp_info}{Information regarding the compound’s classification, formula, and additional relevant data.}
#'   \item{other}{Other fields may also be included, depending on the specific compound and the data available from the RCSB PDB.}
#' }
#' @importFrom httr GET http_status content
#' @importFrom jsonlite fromJSON
#' @examples
#' \dontrun{
#' # Retrieve chemical information for N-Acetyl-D-Glucosamine (NAG)
#' chem_desc <- describe_chemical('NAG')
#'
#' # Access the SMILES string of the compound
#' smiles_string <- chem_desc$rcsb_chem_comp_descriptor$smiles
#'
#' # Attempting to retrieve a chemical with an invalid ID (e.g., longer than 3 characters)
#' # This will raise an error
#' chem_desc <- describe_chemical('INVALID')
#' }
#' @export


describe_chemical <- function(chem_id) {
  # Validate the input chemical ID
  if (nchar(chem_id) > 3) {
    stop("Input Error: The provided ligand ID '", chem_id, "' exceeds the maximum allowed length of 3 characters. Please provide a valid 3-character ligand ID.")
  }

  # Construct the URL for the API request
  url_root <- 'https://data.rcsb.org/rest/v1/core/chemcomp/'
  url <- paste0(url_root, chem_id)

  # Attempt to retrieve data from the API
  response <- tryCatch(
    {
      GET(url)
    },
    error = function(e) {
      stop("Network Error: Failed to retrieve data for ligand ID '", chem_id, "'. The error encountered was: ", e$message, ". Please check your internet connection or try again later.")
    }
  )

  # Check if the response is NULL
  if (is.null(response)) {
    stop("Server Error: Received a NULL response from the server for ligand ID '", chem_id, "'. The query may have failed or the server may be down.")
  }

  # Check if the HTTP status of the response indicates a failure
  if (http_status(response)$category != "Success") {
    stop("HTTP Error: Retrieval of data for ligand ID '", chem_id, "' failed with status: '", http_status(response)$reason, "'. Please verify the ligand ID and try again.")
  }

  # Attempt to parse the JSON content of the response
  out <- tryCatch(
    {
      fromJSON(content(response, "text", encoding = "UTF-8"))
    },
    error = function(e) {
      stop("Parsing Error: Failed to parse the JSON response for ligand ID '", chem_id, "'. The error encountered was: ", e$message, ". The response might be malformed or unexpected.")
    }
  )

  # Return the parsed output
  return(out)
}


